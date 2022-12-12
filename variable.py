from typing import Union, Optional


class Function:
    """The object to pack function with their closures, normal function will not
    not have closure, so it will just be a empty dict; however, lambda function
    will have closure, which will be included in the object
    """
    def __init__(self, ip: int = -1, closure: Optional[list[dict[str, 'Variable']]] = None):
        self.ip = ip
        if closure is None:
            self.closure = [{}]
        else:
            self.closure = closure


class Variable:
    """The object to pack variable with type and its value. As long as you pass by 
    object reference, everything will be a ref when doing pass by reference. 
    Otherwise, just create a new varaible when passing parameters.
    """

    def __init__(self, type: str, value: Union[int, str, bool, dict[str, 'Variable'], Function]):
        self.type = type
        self.value = value

    def get(self) -> tuple[str, Union[int, str, bool, Function, dict[str, 'Variable']]]:
        return (self.type, self.value)
