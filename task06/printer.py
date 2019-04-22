import model


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self, indent=0, is_statement=True):
        self.indent = indent
        self.is_statement = is_statement

    def add_leading_spaces(self):
        if self.is_statement:
            return "\t" * self.indent
        else:
            return ""

    def get_line(self, elem):
        result = ""
        if self.is_statement:
            result += "\t" * self.indent
        result += elem
        if self.is_statement:
            result += ";\n"
        return result

    def add_list_with_indent(self, lst):
        result = ""
        self.indent += 1
        for exp in lst:
            result += self.format_(exp)
        self.indent -= 1
        return result

    def visit_number(self, number):
        return self.get_line(str(number.value))

    def visit_conditional(self, conditional):
        result = self.add_leading_spaces()
        result += "if ("
        result += self.format_(conditional.condition, is_instruction=False)
        result += ") {\n"
        if conditional.if_true:
            result += self.add_list_with_indent(conditional.if_true)
        result += self.add_leading_spaces() + "}"
        if conditional.if_false:
            result += " else {" + "\n"
            result += self.add_list_with_indent(conditional.if_false)
            result += self.add_leading_spaces() + "}"
        result += "\n"
        return result

    def visit_print(self, print_):
        result = "print " + self.format_(print_.expr, is_instruction=False)
        return self.get_line(result)

    def visit_read(self, read):
        return self.get_line("read " + str(read.name))

    def visit_reference(self, reference):
        return self.get_line(reference.name)

    def visit_function_definition(self, function_definition):
        result = self.add_leading_spaces()
        result += "def " + function_definition.name
        result += "("
        for elem in function_definition.function.args[0:-1]:
            result += elem + ', '
        if function_definition.function.args:
            result += function_definition.function.args[-1]
        result += ") {\n"
        result += self.add_list_with_indent(function_definition.function.body)
        result += self.add_leading_spaces() + "}\n"
        return result

    def visit_function_call(self, function_call):
        result = self.format_(function_call.fun_expr, is_instruction=False)
        result += "("
        for elem in function_call.args[0:-1]:
            result += self.format_(elem, is_instruction=False)
            result += ", "
        if function_call.args:
            result += self.format_(function_call.args[-1],
                                   is_instruction=False)
        result += ")"
        return self.get_line(result)

    def visit_binary_operation(self, binary_operation):
        result = "("
        result += self.format_(binary_operation.lhs, is_instruction=False)
        result += " " + binary_operation.op + " "
        result += self.format_(binary_operation.rhs, is_instruction=False)
        result += ")"
        return self.get_line(result)

    def visit_unary_operation(self, unary_operation):
        result = "(" + unary_operation.op
        result += self.format_(unary_operation.expr, is_instruction=False)
        result += ")"
        return self.get_line(result)

    def format_(self, program, *, is_instruction=True):
        new_printer = PrettyPrinter(self.indent, is_instruction)
        return program.accept(new_printer)


def pretty_print(program):
    printer = PrettyPrinter()
    print(printer.format_(program, is_instruction=True), end="")
