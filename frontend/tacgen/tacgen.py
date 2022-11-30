from cmath import exp
from utils.error import DecafGlobalVarBadInitValueError
import utils.riscv as riscv
from frontend.ast import node
from frontend.ast.tree import *
from frontend.ast.visitor import Visitor
from frontend.symbol.varsymbol import VarSymbol
from frontend.type.array import ArrayType
from utils.tac import tacop
from utils.tac.funcvisitor import FuncVisitor
from utils.tac.programwriter import ProgramWriter
from utils.tac.tacinstr import GlobalVar
from utils.tac.tacprog import TACProg
from utils.tac.temp import Temp
from utils.label.funclabel import FuncLabel

"""
The TAC generation phase: translate the abstract syntax tree into three-address code.
"""


class TACGen(Visitor[FuncVisitor, None]):
    def __init__(self) -> None:
        pass

    # Entry of this phase
    def transform(self, program: Program) -> TACProg:
        pw = ProgramWriter(list(program.functions().keys()))
        self.pw = pw
        for child in program.children:
            if isinstance(child, Function):
                if child.body == NULL:
                    continue
                mv = pw.visitFunc(child.ident.value, len(child.params))
                child.accept(self, mv)
                # Remember to call mv.visitEnd after the translation a function.
                mv.visitEnd()
            else:
                if child.init_expr == NULL and len(child.init_array_elements) == 0:
                    globalVar = GlobalVar(child.ident.value, False, array_dim_list=child.array_dim_list, decl=child)
                else:
                    if isinstance(child.init_expr, IntLiteral):
                        globalVar = GlobalVar(child.ident.value, True, child.init_expr.value, array_dim_list=child.array_dim_list, decl=child)
                    elif len(child.init_array_elements): # 全局数组初始化
                        globalVar = GlobalVar(child.ident.value, True, array_dim_list=child.array_dim_list, init_array_elements=child.init_array_elements, decl=child)
                    else:
                        raise DecafGlobalVarBadInitValueError(child.ident.value)
                pw.globalVars.append(globalVar)
        # Remember to call pw.visitEnd before finishing the translation phase.
        return pw.visitEnd()

    def visitFunction(self, func: Function, mv: FuncVisitor) -> None:
        # print("=============== visitFunction in tacgen ====================")
        """
        1. visit parameter list
        2. visit body
        """
        func.params.accept(self, mv)
        # 对于 main func, 初始化所有带有初始化的全局数组
        if mv.func.entry.func == "main":
            for globalVar in self.pw.globalVars:
                if len(globalVar.array_dim_list) and globalVar.init_flag:
                    prod = 1
                    for index in globalVar.array_dim_list:
                        prod *= index.value
                    # fill_n
                    symbol = globalVar.decl.getattr('symbol')

                    addrTemp = mv.freshTemp() # the base addr temp of global var
                    mv.visitLoadGlobalVarSymbol(addrTemp, symbol.name)
                    
                    mv.visitParameter(addrTemp)
                    array_len_temp = mv.visitLoad(prod)  # n
                    mv.visitParameter(array_len_temp)
                    init_val_temp = mv.visitLoad(0)  # 0
                    mv.visitParameter(init_val_temp)
                    
                    func_temp = mv.freshTemp()
                    mv.visitCall(func_temp, FuncLabel('fill_n'))
                    
                    for i, ele in enumerate(globalVar.init_array_elements):
                        offset = mv.visitLoad(4 * i) # 计算偏移量的 temp
                        new_addrTemp = mv.visitBinary(tacop.BinaryOp.ADD, addrTemp, offset)
                        rhs = mv.visitLoad(ele.value)
                        mv.visitStoreGlobalVarAddr(new_addrTemp, rhs, offset)
        
        func.body.accept(self, mv)
        
    def visitParameterList(self, params: ParameterList, mv: FuncVisitor) -> None:
        # print("=============== visitParameterList in tacgen ====================")
        """
        1. visit every param
        """
        for param in params:
            param.accept(self, mv)
            
    def visitParameter(self, param: Parameter, mv: FuncVisitor) -> None:
        # print("=============== visitParameter in tacgen ====================")
        """
        1. Get the 'symbol' attribute of param.
        2. Use mv.freshTemp to get a new temp variable for this symbol.
        """
        symbol = param.getattr('symbol')
        symbol.temp = mv.freshTemp()
        mv.visitAssignment(symbol.temp, symbol.temp)
        
    def visitCall(self, call: Call, mv: FuncVisitor) -> None:
        # print("=============== visitCall in tacgen ====================")
        """
        1. visit argument list
        """
        call.argument_list.accept(self, mv)
        call.setattr('val', call.argument_list.getattr('val'))

    def visitExpressionList(self, exprs: ExpressionList, mv: FuncVisitor) -> None:
        # print("=============== visitExpressionList in tacgen ====================")
        """
        1. visit every expr
        2. PARAM to func
        3. get func label
        4. fresh a temp to func
        5. set val of exprs
        """
        funcSymbol = exprs.getattr('funcSymbol')
        for expr in exprs:
            expr.accept(self, mv)
        for expr in exprs:
            mv.visitParameter(expr.getattr('val'))
        funcLabel = mv.ctx.getFuncLabel(funcSymbol.name)
        funcSymbol.temp = mv.freshTemp()
        mv.visitCall(funcSymbol.temp, funcLabel)
        exprs.setattr('val', funcSymbol.temp)

    def visitBlock(self, block: Block, mv: FuncVisitor) -> None:
        # print("=============== visitBlock in tacgen ====================")
        for child in block:
            child.accept(self, mv)

    def visitReturn(self, stmt: Return, mv: FuncVisitor) -> None:
        # print("=============== visitReturn in tacgen ====================")
        stmt.expr.accept(self, mv)
        mv.visitReturn(stmt.expr.getattr("val"))

    def visitBreak(self, stmt: Break, mv: FuncVisitor) -> None:
        mv.visitBranch(mv.getBreakLabel())
    
    def visitContinue(self, stmt: Continue, mv: FuncVisitor) -> None:
        mv.visitBranch(mv.getContinueLabel())

    def visitIdentifier(self, ident: Identifier, mv: FuncVisitor) -> None:
        """
        0. tell whether the identifier is a global var(by getting its symbol, isGlobal())
        1. Set the 'val' attribute of ident as the temp variable of the 'symbol' attribute of ident.
        """
        # print("=============== visitIdentifier in tacgen ====================")
        symbol = ident.getattr('symbol')
        if symbol.isGlobal:
            addrTemp = mv.freshTemp() # the base addr temp of global var
            mv.visitLoadGlobalVarSymbol(addrTemp, symbol.name)
            valueTemp = mv.freshTemp() # the value temp of global var
            if not symbol.is_array_symbol: # 普通变量
                mv.visitLoadGlobalVarAddr(valueTemp, addrTemp, None)
                ident.setattr('val', valueTemp)
            else: # array
                ident.setattr('val', addrTemp)
        else:
            ident.setattr('val', ident.getattr('symbol').temp)

    def visitDeclaration(self, decl: Declaration, mv: FuncVisitor) -> None:
        """
        1. Get the 'symbol' attribute of decl.
        2. Use mv.freshTemp to get a new temp variable for this symbol.
        3. If the declaration has an initial value, use mv.visitAssignment to set it.
        """
        # print("=============== visitDeclaration in tacgen ====================")
        
        symbol = decl.getattr('symbol')
        symbol.temp = mv.freshTemp()
        
        if decl.array_dim_list == NULL:
            # if not hasattr(symbol, 'temp'):
            if decl.init_expr != NULL: # with initial value
                decl.init_expr.accept(self, mv)
                mv.visitAssignment(symbol.temp, decl.init_expr.getattr("val"))
            # decl.ident.accept(self, mv)
        else:
            prod = 1
            for index in decl.array_dim_list:
                prod *= index.value
            mv.visitAllocForArray(symbol.temp, prod * 4)
            # fill_n
            mv.visitParameter(symbol.temp)
            array_len_temp = mv.visitLoad(prod)  # n
            mv.visitParameter(array_len_temp)
            init_val_temp = mv.visitLoad(0)  # 0
            mv.visitParameter(init_val_temp)
            
            func_temp = mv.freshTemp()
            mv.visitCall(func_temp, FuncLabel('fill_n'))
            
            for i, ele in enumerate(decl.init_array_elements):
                offset = mv.visitLoad(4 * i) # 计算偏移量的 temp
                new_addrTemp = mv.visitBinary(tacop.BinaryOp.ADD, symbol.temp, offset)
                rhs = mv.visitLoad(ele.value)
                mv.visitStoreGlobalVarAddr(new_addrTemp, rhs, offset)


    def visitAssignment(self, expr: Assignment, mv: FuncVisitor) -> None:
        """
        0. tell whether the lhs is a global var
        1. Visit the right hand side of expr, and get the temp variable of left hand side.
        2. Use mv.visitAssignment to emit an assignment instruction.
        3. Set the 'val' attribute of expr as the value of assignment instruction.
        """
        # print('============= visit assignment ==================')
        
        symbol = expr.lhs.getattr('symbol')
        expr.lhs.setattr('lhs', True)
        if len(symbol.array_dim_list) == 0:
            if symbol.isGlobal:
                expr.rhs.accept(self, mv)
                addrTemp = mv.freshTemp() # the base addr temp of global var
                mv.visitLoadGlobalVarSymbol(addrTemp, symbol.name)
                mv.visitStoreGlobalVarAddr(addrTemp, expr.rhs.getattr("val"))
                expr.setattr('val', expr.rhs.getattr("val"))
            else:
                expr.lhs.accept(self, mv)
                expr.rhs.accept(self, mv)
                temp = expr.lhs.getattr("val")
                mv.visitAssignment(temp, expr.rhs.getattr("val"))
                expr.setattr('val', expr.rhs.getattr("val"))
        else:
            if symbol.isGlobal:
                expr.lhs.accept(self, mv)
                expr.rhs.accept(self, mv)
                addrTemp = mv.freshTemp() # the base addr temp of global var
                mv.visitLoadGlobalVarSymbol(addrTemp, symbol.name)
                addrTemp = mv.visitBinary(tacop.BinaryOp.ADD, addrTemp, expr.lhs.getattr("offset"))
                mv.visitStoreGlobalVarAddr(addrTemp, expr.rhs.getattr("val"), expr.lhs.getattr("offset"))
                expr.setattr('val', expr.rhs.getattr("val"))
            else:
                expr.lhs.accept(self, mv)
                expr.rhs.accept(self, mv)
                addrTemp = mv.visitBinary(tacop.BinaryOp.ADD, symbol.temp, expr.lhs.getattr("offset"))
                mv.visitStoreGlobalVarAddr(addrTemp, expr.rhs.getattr("val"), expr.lhs.getattr("offset"))
                expr.setattr('val', expr.rhs.getattr("val"))

    def visitArrayElement(self, arrayElement: ArrayElement, mv: FuncVisitor) -> None:
        """
        0. symbol.temp is the base addr of array, for local one
        1. arrayElement.array_dim_list (int or ident), symbol.array_dim_list (int)
            1.1 ident.getattr("val") is the temp of ident
        2. whatever global or not, calculate the offset
        3. for global one
            3.1 lhs: only offset
            3.2 rhs: offset + value
        4. for local one
            4.1 lhs: only offset
            4.2 rhs: offset + value
        """
        # print("=============== visitArrayElement in tacgen ====================")
        symbol = arrayElement.ident.getattr('symbol')
        prod = mv.visitLoad(1) # 累乘值的 temp
        offset = mv.visitLoad(0) # 计算偏移量的 temp
        length = len(symbol.array_dim_list)
        for (i, index) in enumerate(symbol.array_dim_list):
            arrayElement.array_dim_list[length - 1 - i].accept(self, mv)
            symbol.array_dim_list[length - 1 - i].accept(self, mv)
            
            mul_temp = mv.visitBinary(tacop.BinaryOp.MUL, prod, arrayElement.array_dim_list[length - 1 - i].getattr('val'))
            offset = mv.visitBinary(tacop.BinaryOp.ADD, offset, mul_temp)
            
            prod = mv.visitBinary(tacop.BinaryOp.MUL, prod, symbol.array_dim_list[length - 1 - i].getattr('val'))

        temp_4 = mv.visitLoad(4)
        offset = mv.visitBinary(tacop.BinaryOp.MUL, offset, temp_4)
        arrayElement.setattr('offset', offset)
        
        if symbol.isGlobal:
            if not arrayElement.getattr('lhs'):
                addrTemp = mv.freshTemp() # the base addr temp of global var
                mv.visitLoadGlobalVarSymbol(addrTemp, symbol.name)
                valueTemp = mv.freshTemp() # the value temp of global var
                addrTemp = mv.visitBinary(tacop.BinaryOp.ADD, addrTemp, offset)
                mv.visitLoadGlobalVarAddr(valueTemp, addrTemp, offset)
                arrayElement.setattr('val', valueTemp)
        else:
            if not arrayElement.getattr('lhs'): # 不是左值，就去 load
                valueTemp = mv.freshTemp() # the value temp of arrayElement
                addrTemp = mv.visitBinary(tacop.BinaryOp.ADD, symbol.temp, offset)
                mv.visitLoadGlobalVarAddr(valueTemp, addrTemp, offset)
                arrayElement.setattr('val', valueTemp)

    def visitIf(self, stmt: If, mv: FuncVisitor) -> None:
        # print('============= visit if ==================')
        stmt.cond.accept(self, mv)

        if stmt.otherwise is NULL:
            skipLabel = mv.freshLabel()
            mv.visitCondBranch(
                tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), skipLabel
            )
            stmt.then.accept(self, mv)
            mv.visitLabel(skipLabel)
        else:
            skipLabel = mv.freshLabel()
            exitLabel = mv.freshLabel()
            mv.visitCondBranch(
                tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), skipLabel
            )
            stmt.then.accept(self, mv)
            mv.visitBranch(exitLabel)
            mv.visitLabel(skipLabel)
            stmt.otherwise.accept(self, mv)
            mv.visitLabel(exitLabel)

    def visitWhile(self, stmt: While, mv: FuncVisitor) -> None:
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)

        mv.visitLabel(beginLabel)
        stmt.cond.accept(self, mv)
        mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)

        stmt.body.accept(self, mv)
        mv.visitLabel(loopLabel)
        mv.visitBranch(beginLabel)
        mv.visitLabel(breakLabel)
        mv.closeLoop()

    def visitDoWhile(self, stmt: DoWhile, mv: FuncVisitor) -> None:
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)

        mv.visitLabel(beginLabel)
        stmt.body.accept(self, mv)

        stmt.cond.accept(self, mv)
        mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)
        mv.visitLabel(loopLabel)
        mv.visitBranch(beginLabel)
        mv.visitLabel(breakLabel)
        mv.closeLoop()

    def visitFor(self, stmt: For, mv: FuncVisitor) -> None:
        # print('============= visit for in tacgen ==================')
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)
        stmt.init.accept(self, mv)
        mv.visitLabel(beginLabel)
        stmt.cond.accept(self, mv)
        mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)

        stmt.body.accept(self, mv)
        mv.visitLabel(loopLabel)

        stmt.update.accept(self, mv)
        mv.visitBranch(beginLabel)
        mv.visitLabel(breakLabel)
        mv.closeLoop()

    def visitUnary(self, expr: Unary, mv: FuncVisitor) -> None:
        expr.operand.accept(self, mv)

        op = {
            node.UnaryOp.Neg: tacop.UnaryOp.NEG,
            node.UnaryOp.BitNot: tacop.UnaryOp.NOT,
            node.UnaryOp.LogicNot: tacop.UnaryOp.SEQZ,
            # You can add unary operations here.
        }[expr.op]
        expr.setattr("val", mv.visitUnary(op, expr.operand.getattr("val")))

    def visitBinary(self, expr: Binary, mv: FuncVisitor) -> None:
        # print('============= visit binary ==================')
        expr.lhs.accept(self, mv)
        expr.rhs.accept(self, mv)

        op = {
            node.BinaryOp.Add: tacop.BinaryOp.ADD,
            node.BinaryOp.Sub: tacop.BinaryOp.SUB,
            node.BinaryOp.Mul: tacop.BinaryOp.MUL,
            node.BinaryOp.Div: tacop.BinaryOp.DIV,
            node.BinaryOp.Mod: tacop.BinaryOp.REM,
            node.BinaryOp.LogicAnd: tacop.BinaryOp.LOGICAND,
            node.BinaryOp.BitAnd: tacop.BinaryOp.AND,
            node.BinaryOp.BitOr: tacop.BinaryOp.OR,
            node.BinaryOp.LogicOr: tacop.BinaryOp.LOGICOR,
            node.BinaryOp.EQ: tacop.BinaryOp.EQU,
            node.BinaryOp.NE: tacop.BinaryOp.NEQ,
            node.BinaryOp.GE: tacop.BinaryOp.GEQ,
            node.BinaryOp.LE: tacop.BinaryOp.LEQ,
            node.BinaryOp.GT: tacop.BinaryOp.SGT,
            node.BinaryOp.LT: tacop.BinaryOp.SLT,
            # node.BinaryOp.Assign: tacop.BinaryOp.
            # You can add binary operations here.
        }[expr.op]
        expr.setattr(
            "val", mv.visitBinary(op, expr.lhs.getattr("val"), expr.rhs.getattr("val"))
        )

    def visitCondExpr(self, expr: ConditionExpression, mv: FuncVisitor) -> None:
        """
        1. Refer to the implementation of visitIf and visitBinary.
        """
        # print('============= visit condExpr ==================')
        expr.cond.accept(self, mv)

        skipLabel = mv.freshLabel()
        exitLabel = mv.freshLabel()
        mv.visitCondBranch(
            tacop.CondBranchOp.BEQ, expr.cond.getattr("val"), skipLabel
        )

        expr.then.accept(self, mv)
        mv.visitBranch(exitLabel)
        mv.visitLabel(skipLabel)

        expr.otherwise.accept(self, mv)
        mv.visitAssignment(expr.then.getattr("val"), expr.otherwise.getattr("val"))
        mv.visitLabel(exitLabel)

        expr.setattr("val", expr.then.getattr("val"))

    def visitIntLiteral(self, expr: IntLiteral, mv: FuncVisitor) -> None:
        expr.setattr("val", mv.visitLoad(expr.value))
