from backend.dataflow.cfg import CFG
from backend.dataflow.cfgbuilder import CFGBuilder
from backend.dataflow.livenessanalyzer import LivenessAnalyzer
from backend.reg.bruteregalloc import BruteRegAlloc
from backend.riscv.riscvasmemitter import RiscvAsmEmitter
from utils.tac.tacprog import TACProg

"""
Asm: we use it to generate all the asm code for the program
"""

class Asm:
    def __init__(self, emitter: RiscvAsmEmitter, regAlloc: BruteRegAlloc) -> None:
        self.emitter = emitter
        self.regAlloc = regAlloc

    def transform(self, prog: TACProg):
        analyzer = LivenessAnalyzer()
        
        self.emitter.emitGlobalVars(prog.globalVars)
        self.emitter.emitText()
        
        for func in prog.funcs:
            self.emitter.array_offset = 0 # clear the offset for local array
            pair = self.emitter.selectInstr(func)
            builder = CFGBuilder()
            # from IPython import embed
            # embed()
            cfg: CFG = builder.buildFrom(pair[0])
            analyzer.accept(cfg)
            self.regAlloc.accept(cfg, pair[1])

        return self.emitter.emitEnd()
