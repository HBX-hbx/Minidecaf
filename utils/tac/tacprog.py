from typing import Any, Optional, Union

from utils.tac.tacinstr import GlobalVar

from .tacfunc import TACFunc


# A TAC program consists of several TAC functions.
class TACProg:
    def __init__(self, funcs: list[TACFunc], globalVars: list[GlobalVar]) -> None:
        self.funcs = funcs
        self.globalVars = globalVars

    def printTo(self) -> None:
        for globalVar in self.globalVars:
            print(str(globalVar))
        for func in self.funcs:
            func.printTo()
