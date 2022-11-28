from utils.tac.temp import Temp

from .symbol import *

"""
Variable symbol, representing a variable definition.
"""


class VarSymbol(Symbol):
    def __init__(self, name: str, type: DecafType, isGlobal: bool = False, array_dim_list: list = []) -> None:
        super().__init__(name, type)
        self.temp: Temp
        self.isGlobal = isGlobal
        self.initValue = 0
        self.array_dim_list = array_dim_list
        self.is_array_symbol = False

    def __str__(self) -> str:
        return "variable %s : %s" % (self.name, str(self.type))

    # To set the initial value of a variable symbol (used for global variable).
    def setInitValue(self, value: int) -> None:
        self.initValue = value
