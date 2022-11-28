from typing import Protocol, TypeVar, cast

from frontend.ast.node import Node, NullType
from frontend.ast.tree import *
from frontend.ast.visitor import RecursiveVisitor, Visitor
from frontend.scope.globalscope import GlobalScope
from frontend.scope.scope import Scope, ScopeKind
from frontend.scope.scopestack import ScopeStack
from frontend.symbol.funcsymbol import FuncSymbol
from frontend.symbol.symbol import Symbol
from frontend.symbol.varsymbol import VarSymbol
from frontend.type.array import ArrayType
from frontend.type.type import DecafType
from utils.error import *
from utils.riscv import MAX_INT

"""
The namer phase: resolve all symbols defined in the abstract syntax tree and store them in symbol tables (i.e. scopes).
"""


class Namer(Visitor[ScopeStack, None]):
    def __init__(self) -> None:
        pass

    # Entry of this phase
    def transform(self, program: Program) -> Program:
        # Global scope. You don't have to consider it until Step 9.
        program.globalScope = GlobalScope
        ctx = ScopeStack(program.globalScope)

        program.accept(self, ctx)
        return program

    def visitProgram(self, program: Program, ctx: ScopeStack) -> None:
        # Check if the 'main' function is missing
        # print("=============== visitProgram in Namer ====================")
        if not program.hasMainFunc():
            raise DecafNoMainFuncError
        """
        1. traverse all the functions and declarations, set global attribute for declaration
        2. func.accept()
        """
        for child in program.children:
            if isinstance(child, Declaration):
                child.setattr('global', True)
            child.accept(self, ctx)
        # for symbol in ctx.globalscope.symbols.values():
        #     if symbol.isFunc:
        #         if not symbol.isDefined:
        #             raise DecafUndefinedFuncError(symbol.name)

    def visitFunction(self, func: Function, ctx: ScopeStack) -> None:
        # print("=============== visitFunction in Namer ====================")
        """
        1. whether the func with the same name has been declared
            1.1 If yes, get the funcSymbol, whether has been defined
                1.1.1 If yes, raise
                1.1.2 If not, whether the func.body exists
                    1.1.2.1 If yes, check params, if not the same, raise, else going on
        2. build a new FuncSymbol, and put it into the global scope, and open a local scope
        3. Set the 'symbol' attribute of func. (has been declared)
        4. visit the parameter list and the ident
        5. check whether func.body exists
            5.1 If yes, visit it, and set 'isDefined' attr to True
            5.2 If not, set 'isDefined' attr to False
        6. close the scope
        """
        
        if ctx.globalscope.containsKey(func.ident.value): # has been declared
            funcSymbol = ctx.lookup(func.ident.value)
            if funcSymbol.isDefined: # has been defined
                raise DecafDeclConflictError(func.ident.value)
            else: # has not been defined
                if func.body != NULL:
                    # check lens
                    if funcSymbol.parameterNum != len(func.params):
                        raise DecafDeclConflictError(func.ident.value)
                    else:
                        # check types
                        # for i in range(len(func.params)):
                        #     # TODO: whether to shadow the ident previous declaration
                        #     if (func.params[i].var_t != funcSymbol.getParaType(i)):
                        #         raise DecafTypeMismatchError()
                        ctx.open(Scope(ScopeKind.LOCAL))
                        func.params.setattr('funcSymbol', funcSymbol)
                        func.params.accept(self, ctx)
                        for child in func.body:
                            child.accept(self, ctx)
                        funcSymbol.isDefined = True
                        ctx.close()
                # else:
                #     raise DecafDeclConflictError(func.ident.value)
        else: # not been declared
            funcSymbol = FuncSymbol(func.ident.value, func.ret_t.type, ctx.globalscope)
            ctx.declareGlobal(funcSymbol)
            ctx.open(Scope(ScopeKind.LOCAL))
            func.setattr('funcSymbol', funcSymbol)
            func.ident.accept(self, ctx)
            func.ident.setattr('funcSymbol', funcSymbol)
            func.params.setattr('funcSymbol', funcSymbol)
            func.params.accept(self, ctx)

            if func.body != NULL: # definition
                for child in func.body:
                    child.accept(self, ctx)
                funcSymbol.isDefined = True

            else: # declaration
                funcSymbol.isDefined = False
            
            ctx.close()
        
    def visitCall(self, call: Call, ctx: ScopeStack) -> Optional[U]:
        # print("=============== visitCall in Namer ====================")
        """
        0. open local scope
        1. get the funcSymbol, check whether it is None (not declared) and whether it is undefined (not defined)
        2. if defined, check whether len(call.argument_list) == len(func.params) and the var_type
        """
        ctx.open(Scope(ScopeKind.LOCAL))

        funcSymbol = ctx.lookup(call.ident.value)
        if funcSymbol != None: # declared
            # check params lens
            if funcSymbol.parameterNum != len(call.argument_list):
                raise DecafBadFuncCallError(call.ident.value)
            else:
                call.ident.accept(self, ctx)
                call.setattr('funcSymbol', funcSymbol)
                call.argument_list.setattr('funcSymbol', funcSymbol)
                call.argument_list.accept(self, ctx)
        else: # not declared
            raise DecafUndefinedFuncError(call.ident.value)
    
        ctx.close()
    
    def visitExpressionList(self, exprs: ExpressionList, ctx: T) -> None:
        # print("=============== visitExpressionList in Namer ====================")
        """
        1. visit all the exprs and set the funcSymbol
        """
        symbol = exprs.getattr("funcSymbol")
        for expr in exprs:
            expr.setattr('funcSymbol', symbol)
            expr.accept(self, ctx)
    
    def visitParameterList(self, params: ParameterList, ctx: ScopeStack) -> None:
        # print("=============== visitParameterList in Namer ====================")
        """
        1. visit all the params and set the funcSymbol
        """
        symbol = params.getattr("funcSymbol")
        add = True
        if symbol.parameterNum > 0: # have added
            add = False
        for param in params:
            param.setattr('funcSymbol', symbol)
            param.setattr('add', add)
            param.accept(self, ctx)

    def visitParameter(self, param: Parameter, ctx: ScopeStack) -> None:
        # print("=============== visitParameter in Namer ====================")
        """
        0. if ident not exists, just add type to the func symbol
        1. Use ctx.findConflict to find if a param with the same name has been declared.
        2. If not, build a new VarSymbol, and put it into the current scope using ctx.declare.
        3. Set the 'symbol' attribute of param.
        4. visit the ident of param
        5. add the type to the func symbol
        """
        if param.ident != NULL:
            symbol = ctx.findConflict(param.ident.value)
            if symbol == None: # has not been declared
                symbol = VarSymbol(param.ident.value, param.var_t.type)
                ctx.declare(symbol)
            else:
                raise DecafDeclConflictError(param.ident.value)
            param.setattr('symbol', symbol)
            param.ident.accept(self, ctx)
        if param.getattr('add'):
            param.getattr("funcSymbol").addParaType(param.var_t)

    def visitBlock(self, block: Block, ctx: ScopeStack) -> None:
        # print("=============== visitBlock in Namer ====================")
        ctx.open(Scope(ScopeKind.LOCAL))
        for child in block:
            child.accept(self, ctx)
        ctx.close()

    def visitReturn(self, stmt: Return, ctx: ScopeStack) -> None:
        # print("=============== visitReturn in Namer ====================")
        stmt.expr.accept(self, ctx)

    def visitFor(self, stmt: For, ctx: ScopeStack) -> None:
        """
        1. Open a local scope for stmt.init.
        2. Visit stmt.init, stmt.cond, stmt.update.
        3. Open a loop in ctx (for validity checking of break/continue)
        4. Visit body of the loop.
        5. Close the loop and the local scope.
        """
        # print("=============== visitFor in Namer ====================")

        ctx.open(Scope(ScopeKind.LOCAL)) # local scope

        stmt.init.accept(self, ctx)
        stmt.cond.accept(self, ctx)
        stmt.update.accept(self, ctx)

        ctx.openLoop()
        stmt.body.accept(self, ctx)
        ctx.closeLoop()

        ctx.close()

    def visitIf(self, stmt: If, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        stmt.then.accept(self, ctx)

        # check if the else branch exists
        if not stmt.otherwise is NULL:
            stmt.otherwise.accept(self, ctx)

    def visitWhile(self, stmt: While, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        ctx.openLoop()
        stmt.body.accept(self, ctx)
        ctx.closeLoop()

    def visitDoWhile(self, stmt: DoWhile, ctx: ScopeStack) -> None:
        """
        1. Open a loop in ctx (for validity checking of break/continue)
        2. Visit body of the loop.
        3. Close the loop.
        4. Visit the condition of the loop.
        """
        ctx.openLoop()
        stmt.body.accept(self, ctx)
        ctx.closeLoop()
        stmt.cond.accept(self, ctx)

    def visitBreak(self, stmt: Break, ctx: ScopeStack) -> None:
        if not ctx.inLoop():
            raise DecafBreakOutsideLoopError()

    def visitContinue(self, stmt: Continue, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBreak.
        """
        if not ctx.inLoop():
            raise DecafContinueOutsideLoopError()

    def visitDeclaration(self, decl: Declaration, ctx: ScopeStack) -> None:
        """
        1. Use ctx.findConflict to find if a variable with the same name has been declared.
        2. If not, build a new VarSymbol, and put it into the current scope using ctx.declare.
        3. Set the 'symbol' attribute of decl.
        4. If there is an initial value, visit it.
        """
        # print("=============== visitDeclaration in Namer ====================")
        symbol = ctx.findConflict(decl.ident.value)
        if symbol == None: # has not been declared
            # if it is an array, check the dim of array_dim_list, should not be non-positive
            # ident.is_array_ident = True
            if len(decl.array_dim_list):
                decl.ident.is_array_ident = True
                for index in decl.array_dim_list:
                    if index.value <= 0:
                        raise DecafBadArraySizeError
            symbol = VarSymbol(decl.ident.value, decl.var_t.type, array_dim_list=decl.array_dim_list)
            symbol.is_array_symbol = decl.ident.is_array_ident
            
            if decl.getattr('global'):
                symbol.isGlobal = True
                ctx.declareGlobal(symbol)
            else:
                ctx.declare(symbol)
        else:
            if decl.getattr('global'):
                raise DecafGlobalVarDefinedTwiceError(decl.ident.value)
            else:
                raise DecafDeclConflictError(decl.ident.value)
        decl.setattr('symbol', symbol)
        decl.init_expr.accept(self, ctx)

    def visitAssignment(self, expr: Assignment, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        # print("=============== visitAssignment in Namer ====================")
        if isinstance(expr.lhs, Identifier) or isinstance(expr.lhs, ArrayElement): # lhs 是左值
            # lhs should be var ident, not array ident
            if isinstance(expr.lhs, Identifier):
                symbol = ctx.lookup(expr.lhs.value)
                if symbol.is_array_symbol:
                    raise DecafBadAssignTypeError
            expr.lhs.accept(self, ctx)
            expr.rhs.accept(self, ctx)
        else:
            raise DecafSyntaxError('left hand side of assignment is not a left value')

    def visitArrayElement(self, arrayElement: ArrayElement, ctx: ScopeStack) -> None:
        """
        1. Use ctx.lookup to find the symbol corresponding to ident.
        2. If it has not been declared, raise a DecafUndefinedVarError.
        3. check whether beyond the dims of the array symbol, or dims not match
        """
        # print("=============== visitArrayElement in Namer ====================")
        symbol = ctx.lookup(arrayElement.ident.value)
        if symbol != None: # has been declared
            if len(arrayElement.array_dim_list) != len(symbol.array_dim_list):
                raise DecafBadIndexError('dim not match!')
            for (i, index) in enumerate(arrayElement.array_dim_list):
                index.accept(self, ctx)
                if isinstance(index, IntLiteral): # 只对 IntLiteral 检查语义
                    if index.value >= symbol.array_dim_list[i].value:
                        raise DecafBadIndexError(str(index.value))
            
            arrayElement.ident.setattr('symbol', symbol)
            arrayElement.setattr('symbol', symbol)
        else: # has not been declared
            raise DecafUndefinedVarError(arrayElement.ident.value)
        pass

    def visitUnary(self, expr: Unary, ctx: ScopeStack) -> None:
        # should be var ident, not array ident
        if isinstance(expr.operand, Identifier):
            symbol = ctx.lookup(expr.operand.value)
            if symbol.is_array_symbol:
                raise DecafBadAssignTypeError
        expr.operand.accept(self, ctx)

    def visitBinary(self, expr: Binary, ctx: ScopeStack) -> None:
        # should be var ident, not array ident
        if isinstance(expr.lhs, Identifier):
            lhs_symbol = ctx.lookup(expr.lhs.value)
            if lhs_symbol.is_array_symbol:
                raise DecafBadAssignTypeError
        if isinstance(expr.rhs, Identifier):
            rhs_symbol = ctx.lookup(expr.rhs.value)
            if rhs_symbol.is_array_symbol:
                raise DecafBadAssignTypeError
        expr.lhs.accept(self, ctx)
        expr.rhs.accept(self, ctx)

    def visitCondExpr(self, expr: ConditionExpression, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        expr.cond.accept(self, ctx)
        expr.then.accept(self, ctx)
        expr.otherwise.accept(self, ctx)

    def visitIdentifier(self, ident: Identifier, ctx: ScopeStack) -> None:
        """
        1. Use ctx.lookup to find the symbol corresponding to ident.
        2. If it has not been declared, raise a DecafUndefinedVarError.
        3. Set the 'symbol' attribute of ident.
        """
        # print("=============== visitIdentifier in Namer ====================")
        symbol = ctx.lookup(ident.value)
        if symbol != None: # has been declared
            ident.setattr('symbol', symbol)
        else: # has not been declared
            raise DecafUndefinedVarError(ident.value)

    def visitIntLiteral(self, expr: IntLiteral, ctx: ScopeStack) -> None:
        value = expr.value
        if value > MAX_INT:
            raise DecafBadIntValueError(value)
