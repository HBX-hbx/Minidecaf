import enum
import random

from backend.dataflow.basicblock import BasicBlock, BlockKind
from backend.dataflow.cfg import CFG
from backend.dataflow.loc import Loc
from backend.reg.regalloc import RegAlloc
from backend.riscv.riscvasmemitter import RiscvAsmEmitter
from backend.subroutineemitter import SubroutineEmitter
from backend.subroutineinfo import SubroutineInfo
from utils.riscv import Riscv
from utils.tac.holeinstr import HoleInstr
from utils.tac.nativeinstr import NativeInstr
from utils.tac.reg import Reg
from utils.tac.temp import Temp

"""
BruteRegAlloc: one kind of RegAlloc

bindings: map from temp.index to Reg

we don't need to take care of GlobalTemp here
because we can remove all the GlobalTemp in selectInstr process

1. accept：根据每个函数的 CFG 进行寄存器分配，寄存器分配结束后生成相应汇编代码
2. bind：将一个 Temp 与寄存器绑定
3. unbind：将一个 Temp 与相应寄存器解绑定
4. localAlloc：根据数据流对一个 BasicBlock 内的指令进行寄存器分配
5. allocForLoc：每一条指令进行寄存器分配
6. allocRegFor：根据数据流决定为当前 Temp 分配哪一个寄存器
"""

class BruteRegAlloc(RegAlloc):
    def __init__(self, emitter: RiscvAsmEmitter) -> None:
        super().__init__(emitter)
        self.bindings = {}
        self.used_args_cnt = 0 # count the number of used args
        self.params = []
        self.reg_to_stack = [] # store regs for put on the stack
        for reg in emitter.allocatableRegs:
            reg.used = False

    def accept(self, graph: CFG, info: SubroutineInfo) -> None:
        # print('------------ accept --------------')
        # from IPython import embed
        # embed()
        subEmitter = self.emitter.emitSubroutine(info)
        for bb in graph.iterator():
            # you need to think more here
            # maybe we don't need to alloc regs for all the basic blocks
            if not graph.reachable(bb.id): # not reachable
                continue
            if bb.label is not None:
                subEmitter.emitLabel(bb.label)
            self.localAlloc(bb, subEmitter)
        subEmitter.emitEnd()

    def bind(self, temp: Temp, reg: Reg):
        reg.used = True
        self.bindings[temp.index] = reg
        reg.occupied = True
        reg.temp = temp

    def unbind(self, temp: Temp):
        if temp.index in self.bindings:
            self.bindings[temp.index].occupied = False
            self.bindings.pop(temp.index)

    def localAlloc(self, bb: BasicBlock, subEmitter: SubroutineEmitter):
        # print('------------ localAlloc --------------')
        
        self.bindings.clear()
        for reg in self.emitter.allocatableRegs:
            reg.occupied = False
        # from IPython import embed
        # embed()
        # in step9, you may need to think about how to store callersaved regs here
        for loc in bb.allSeq():
            subEmitter.emitComment(str(loc.instr))

            self.allocForLoc(loc, subEmitter)

        for tempindex in bb.liveOut:
            if tempindex in self.bindings:
                subEmitter.emitStoreToStack(self.bindings.get(tempindex))

        if (not bb.isEmpty()) and (bb.kind is not BlockKind.CONTINUOUS):
            self.allocForLoc(bb.locs[len(bb.locs) - 1], subEmitter)

    def allocForLoc(self, loc: Loc, subEmitter: SubroutineEmitter):
        # print('------------ allocForLoc --------------')
        instr = loc.instr
        srcRegs: list[Reg] = []
        dstRegs: list[Reg] = []
        # from IPython import embed
        # embed()

        for i in range(len(instr.srcs)):
            temp = instr.srcs[i]
            if isinstance(temp, Reg):
                srcRegs.append(temp)
            else:
                srcRegs.append(self.allocRegFor(temp, True, loc.liveIn, subEmitter))

        for i in range(len(instr.dsts)):
            temp = instr.dsts[i]
            if isinstance(temp, Reg):
                dstRegs.append(temp)
            else:
                dstRegs.append(self.allocRegFor(temp, False, loc.liveIn, subEmitter))
        # print('------------ before emitNative --------------')
        
        '''
        1. tell whether the cnt of args exceeds 8
        2. store in the reg or store on the stack(add the sp reg)
        '''
        if isinstance(instr, Riscv.Param): # example: sw _T0, 0(sp)
            src_reg = srcRegs[0]
            if self.used_args_cnt >= 8: # exceed eight reg args, should be stored on stack
                self.reg_to_stack.append(src_reg)
            else: # reg args
                arg_reg = Riscv.ArgRegs[self.used_args_cnt]
                
                arg_reg.args_occupied = True
                
                subEmitter.emitNative(Riscv.Move(arg_reg, src_reg)) # mv a0, t0
            self.used_args_cnt += 1
            return

        '''
        1. save all caller saved regs
        2. put args to the stack
        '''
        if isinstance(instr, Riscv.Call):
            self.savedRegs = []
            for reg in self.emitter.callerSaveRegs:
                if reg.isUsed():
                    self.savedRegs.append(reg)
                    subEmitter.emitStoreToStack(reg)
                
            if len(self.reg_to_stack):
                subEmitter.emitNative(Riscv.SPAdd(-4 * len(self.reg_to_stack)))
                for i, reg in enumerate(self.reg_to_stack):
                    subEmitter.emitNative(Riscv.NativeStoreWord(self.reg_to_stack[i], Riscv.SP, 4 * i))
            else:
                pass
        
        # if isinstance(instr, Riscv.Call):
        #     from IPython import embed
        #     embed()
        subEmitter.emitNative(instr.toNative(dstRegs, srcRegs))
        '''
        1. get all caller saved regs
        2. add the sp for reg_to_stack
        '''
        if isinstance(instr, Riscv.Call):
            if len(self.reg_to_stack):
                subEmitter.emitNative(Riscv.SPAdd(4 * len(self.reg_to_stack)))
            self.used_args_cnt = 0
            self.reg_to_stack.clear()
            for reg in self.savedRegs:
                if reg != Riscv.A0:
                    subEmitter.emitLoadFromStack(reg, reg.temp)

    def allocRegFor(
        self, temp: Temp, isRead: bool, live: set[int], subEmitter: SubroutineEmitter
    ):
        # print('------------ allocRegFor --------------')
        # from IPython import embed
        # embed()
        if temp.index in self.bindings:
            return self.bindings[temp.index]

        for reg in self.emitter.allocatableRegs:
            if not reg.args_occupied and ((not reg.occupied) or (not reg.temp.index in live)):
                subEmitter.emitComment(
                    "  allocate {} to {}  (read: {}):".format(
                        str(temp), str(reg), str(isRead)
                    )
                )
                if isRead:
                    subEmitter.emitLoadFromStack(reg, temp)
                if reg.occupied:
                    self.unbind(reg.temp)
                self.bind(temp, reg)
                return reg
        reg = self.emitter.allocatableRegs[
            random.randint(0, len(self.emitter.allocatableRegs))
        ]
        subEmitter.emitStoreToStack(reg)
        subEmitter.emitComment("  spill {} ({})".format(str(reg), str(reg.temp)))
        self.unbind(reg.temp)
        self.bind(temp, reg)
        subEmitter.emitComment(
            "  allocate {} to {} (read: {})".format(str(temp), str(reg), str(isRead))
        )
        if isRead:
            subEmitter.emitLoadFromStack(reg, temp)
        return reg
