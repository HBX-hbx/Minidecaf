from enum import Enum, auto, unique
from typing import Any, Optional, Union

from utils.label.label import Label
from utils.tac.nativeinstr import NativeInstr
from utils.tac.reg import Reg

from .tacop import *
from .tacvisitor import TACVisitor
from .temp import Temp


class TACInstr:
    def __init__(
        self,
        kind: InstrKind,
        dsts: list[Temp],
        srcs: list[Temp],
        label: Optional[Label],
    ) -> None:
        self.kind = kind
        self.dsts = dsts.copy()
        self.srcs = srcs.copy()
        self.label = label

    def getRead(self) -> list[int]:
        # try:
        return [src.index for src in self.srcs]
        # except:
        #     from IPython import embed
        #     embed()

    def getWritten(self) -> list[int]:
        return [dst.index for dst in self.dsts]

    def isLabel(self) -> bool:
        return self.kind is InstrKind.LABEL

    def isSequential(self) -> bool:
        return self.kind == InstrKind.SEQ

    def isReturn(self) -> bool:
        return self.kind == InstrKind.RET

    def toNative(self, dstRegs: list[Reg], srcRegs: list[Reg]) -> NativeInstr:
        oldDsts = dstRegs
        oldSrcs = srcRegs
        self.dsts = dstRegs
        self.srcs = srcRegs
        instrString = self.__str__()
        newInstr = NativeInstr(self.kind, dstRegs, srcRegs, self.label, instrString)
        self.dsts = oldDsts
        self.srcs = oldSrcs
        return newInstr

    def accept(self, v: TACVisitor) -> None:
        pass

class Call(TACInstr):
    def __init__(self, dst: Temp, target: Label) -> None:
        super().__init__(InstrKind.SEQ, [dst], [], target)
        self.dst = dst
        self.target = target

    def __str__(self) -> str:
        return "%s = CALL %s" % (self.dst, str(self.target))

    def accept(self, v: TACVisitor) -> None:
        v.visitCall(self)
        

class Param(TACInstr):
    def __init__(self, src: Temp) -> None:
        super().__init__(InstrKind.SEQ, [], [src], None)
        self.src = src

    def __str__(self) -> str:
        return "PARAM %s" % self.src

    def accept(self, v: TACVisitor) -> None:
        v.visitParameter(self)


class GlobalVar(TACInstr):
    def __init__(self, symbol: str, init_flag: bool, init_value: int = 0, array_dim_list: list = []) -> None:
        super().__init__(InstrKind.SEQ, [], [], None)
        self.symbol = symbol
        self.init_value = init_value
        self.init_flag = init_flag
        self.array_dim_list = array_dim_list
        
        prod = 1
        for index in array_dim_list:
            prod *= index.value
        self.cnt_bytes = prod * 4

    def __str__(self) -> str:
        if self.init_flag:
            return "global variable %s = %d" % (self.symbol, self.init_value)
        return "global variable %s" % self.symbol

    def accept(self, v: TACVisitor) -> None:
        v.visitGlobalVar(self)


class LoadGlobalVarSymbol(TACInstr):
    def __init__(self, symbol: str, dst: Temp) -> None:
        super().__init__(InstrKind.SEQ, [dst], [], None)
        self.symbol = symbol

    def __str__(self) -> str:
        return "%s = load global symbol %s" % (self.dsts[0], self.symbol)

    def accept(self, v: TACVisitor) -> None:
        v.visitLoadGlobalVarSymbol(self)


class LoadGlobalVarAddr(TACInstr):
    def __init__(self, src: Temp, dst: Temp, offset: Temp = None) -> None:
        super().__init__(InstrKind.SEQ, [dst], [src], None)
        self.offset = offset

    def __str__(self) -> str: #  load _T0, 4(_T1): load the value store in (4 + _T1) to _T0
        return "load %s, %d(%s)" % (self.dsts[0], 0, self.srcs[0])

    def accept(self, v: TACVisitor) -> None:
        v.visitLoadGlobalVarAddr(self)


class StoreGlobalVarAddr(TACInstr):
    def __init__(self, src: Temp, dst: Temp, offset: Temp = None) -> None:
        super().__init__(InstrKind.SEQ, [dst], [src], None)
        self.offset = offset

    def __str__(self) -> str: # store _T0, 4(_T1): store _T0 to the addr of (4 + _T1)
        return "store %s, %d(%s)" % (self.srcs[0], 0, self.dsts[0])

    def accept(self, v: TACVisitor) -> None:
        v.visitStoreGlobalVarAddr(self)


class AllocForArray(TACInstr):
    def __init__(self, dst: Temp, cnt_bytes: int) -> None:
        super().__init__(InstrKind.SEQ, [dst], [], None)
        self.cnt_bytes = cnt_bytes

    def __str__(self) -> str: # store _T0, 4(_T1): store _T0 to the addr of (4 + _T1)
        return "%s = alloc %s" % (self.dsts[0], str(self.cnt_bytes))

    def accept(self, v: TACVisitor) -> None:
        v.visitAllocForArray(self)


# Assignment instruction.
class Assign(TACInstr):
    def __init__(self, dst: Temp, src: Temp) -> None:
        super().__init__(InstrKind.SEQ, [dst], [src], None)
        self.dst = dst
        self.src = src

    def __str__(self) -> str:
        return "%s = %s" % (self.dst, self.src)

    def accept(self, v: TACVisitor) -> None:
        v.visitAssign(self)


# Loading an immediate 32-bit constant.
class LoadImm4(TACInstr):
    def __init__(self, dst: Temp, value: int) -> None:
        super().__init__(InstrKind.SEQ, [dst], [], None)
        self.dst = dst
        self.value = value

    def __str__(self) -> str:
        return "%s = %d" % (self.dst, self.value)

    def accept(self, v: TACVisitor) -> None:
        v.visitLoadImm4(self)


# Unary operations.
class Unary(TACInstr):
    def __init__(self, op: UnaryOp, dst: Temp, operand: Temp) -> None:
        super().__init__(InstrKind.SEQ, [dst], [operand], None)
        self.op = op
        self.dst = dst
        self.operand = operand

    def __str__(self) -> str:
        opStr = {
            UnaryOp.NEG: "-",
            UnaryOp.NOT: "~",
            UnaryOp.SEQZ: "!",
        }[self.op]
        return "%s = %s %s" % (
            self.dst,
            opStr,
            self.operand,
        )

    def accept(self, v: TACVisitor) -> None:
        v.visitUnary(self)


# Binary Operations.
class Binary(TACInstr):
    def __init__(self, op: BinaryOp, dst: Temp, lhs: Temp, rhs: Temp) -> None:
        super().__init__(InstrKind.SEQ, [dst], [lhs, rhs], None)
        self.op = op
        self.dst = dst
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self) -> str:
        opStr = {
            BinaryOp.ADD: "+",
            BinaryOp.SUB: "-",
            BinaryOp.MUL: "*",
            BinaryOp.DIV: "/",
            BinaryOp.REM: "%",
            BinaryOp.EQU: "==",
            BinaryOp.NEQ: "!=",
            BinaryOp.SLT: "<",
            BinaryOp.LEQ: "<=",
            BinaryOp.SGT: ">",
            BinaryOp.GEQ: ">=",
            BinaryOp.AND: "&",
            BinaryOp.OR: "|",
            BinaryOp.LOGICAND: "&&",
            BinaryOp.LOGICOR: "||"
        }[self.op]
        return "%s = (%s %s %s)" % (self.dst, self.lhs, opStr, self.rhs)

    def accept(self, v: TACVisitor) -> None:
        v.visitBinary(self)


# Branching instruction.
class Branch(TACInstr):
    def __init__(self, target: Label) -> None:
        super().__init__(InstrKind.JMP, [], [], target)
        self.target = target

    def __str__(self) -> str:
        return "branch %s" % str(self.target)

    def accept(self, v: TACVisitor) -> None:
        v.visitBranch(self)


# Branching with conditions.
class CondBranch(TACInstr):
    def __init__(self, op: CondBranchOp, cond: Temp, target: Label) -> None:
        super().__init__(InstrKind.COND_JMP, [], [cond], target)
        self.op = op
        self.cond = cond
        self.target = target

    def __str__(self) -> str:
        return "if (%s %s) branch %s" % (
            self.cond,
            "== 0" if self.op == CondBranchOp.BEQ else "!= 0",
            str(self.target),
        )

    def accept(self, v: TACVisitor) -> None:
        v.visitCondBranch(self)


# Return instruction.
class Return(TACInstr):
    def __init__(self, value: Optional[Temp]) -> None:
        if value is None:
            super().__init__(InstrKind.RET, [], [], None)
        else:
            super().__init__(InstrKind.RET, [], [value], None)
        self.value = value

    def __str__(self) -> str:
        return "return" if (self.value is None) else ("return " + str(self.value))

    def accept(self, v: TACVisitor) -> None:
        v.visitReturn(self)


# Annotation (used for debugging).
class Memo(TACInstr):
    def __init__(self, msg: str) -> None:
        super().__init__(InstrKind.SEQ, [], [], None)
        self.msg = msg

    def __str__(self) -> str:
        return "memo '%s'" % self.msg

    def accept(self, v: TACVisitor) -> None:
        v.visitMemo(self)


# Label (function entry or branching target).
class Mark(TACInstr):
    def __init__(self, label: Label) -> None:
        super().__init__(InstrKind.LABEL, [], [], label)

    def __str__(self) -> str:
        return "%s:" % str(self.label)

    def accept(self, v: TACVisitor) -> None:
        v.visitMark(self)
