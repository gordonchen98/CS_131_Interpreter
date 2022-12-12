from intbase import InterpreterBase
from variable import Variable, Function
from typing import Optional

Scope_Stack = list[dict[str, Variable]]
Functions = dict[str, Function]


def find_variable(variable: str, scope_stack: Scope_Stack) -> tuple[str, Optional[Variable]]:
    """(for assign) Find the variable that is at the most recent scope frame, 
    return None if there is no such variable (rmb, python is object reference, 
    so you can change it directly and will reflect in the scope_stack as well)

    Args:
        variable (str): Name of the variable we want to search
        scope_stack (Scope_Stack): Scope stack to search from

    Returns:
        tuple[str, Optional[Variable]]: Return the object reference to the 
        Variable object; otherwise, return None. Error will be the first string
        in the tuple, name is NAME_ERROR and type is TYPE_ERROR
    """
    name, mem_var = variable, None
    if len(variable.split('.')) == 2:
        name, mem_var = variable.split('.')

    for scope in scope_stack[::-1]:
        if name in scope:
            # trying to access object with its mem_var
            if mem_var is not None:
                type, obj_dict = scope[name].get()
                # if the name is not associated with an object type, throw error
                if type != InterpreterBase.OBJECT_DEF:
                    return ('type', None)

                # create a new variable so dict will have Variable object init
                obj_dict[mem_var] = Variable(        # type: ignore
                    InterpreterBase.OBJECT_DEF, "")
                return ('none', obj_dict[mem_var])   # type: ignore

            return ('none', scope[variable])
    return ('name', None)


def find_variable_or_function(name: str, scope_stack: Scope_Stack, functions: Functions) -> tuple[str, Optional[Variable]]:
    """(for expression) Find the variable that is at the most recent scope frame, 
    return None if there is no such variable, or find the function that is in the
    function jump table

    Args:
        name (str): Name of the variable we want to search
        scope_stack (Scope_Stack): Scope stack to search from
        functions (Functions): Function jump table to search from

    Returns:
        tuple[str, Optional[Variable]]: Return the object reference to the 
        Variable object; otherwise, return None. Error will be the first string
        in the tuple, name is NAME_ERROR and type is TYPE_ERROR
    """
    if name == InterpreterBase.OBJECT_DEF:
        return ("none", Variable(InterpreterBase.OBJECT_DEF, {}))

    mem_var = None
    if len(name.split('.')) == 2:
        name, mem_var = name.split('.')

    for scope in scope_stack[::-1]:
        if name in scope:
            if mem_var is not None:
                type, obj_dict = scope[name].get()
                # if the name is not associated with an object type, throw error
                if type != InterpreterBase.OBJECT_DEF:
                    return ('type', None)

                if mem_var not in obj_dict:         # type: ignore
                    return ('name', None)

                return ("none", obj_dict[mem_var])  # type: ignore
            return ("none", scope[name])

    if name in functions:
        return ("none", Variable(InterpreterBase.FUNC_DEF, functions[name]))

    return ("name", None)
