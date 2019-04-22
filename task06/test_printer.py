from printer import *
from model import *
from textwrap import dedent
import pytest


def test_conditional():
    program = Conditional(Number(42), [], [])
    result = program.accept(PrettyPrinter())
    assert result == "if (42) {\n}\n"


def test_function_definition():
    program = FunctionDefinition("foo", Function([], []))
    result = program.accept(PrettyPrinter())
    assert result == "def foo() {\n}\n"


def test_print():
    program = Print(Number(42))
    result = program.accept(PrettyPrinter())
    assert result == "print 42;\n"


def test_read():
    program = Read("x")
    result = program.accept(PrettyPrinter())
    assert result == "read x;\n"


def test_number():
    program = Number(10)
    result = program.accept(PrettyPrinter())
    assert result == "10;\n"


def test_reference():
    program = Reference("x")
    result = program.accept(PrettyPrinter())
    assert result == "x;\n"


def test_binary_operation():
    add = BinaryOperation(Number(2), "+", Number(3))
    program = BinaryOperation(Number(1), "*", add)
    result = program.accept(PrettyPrinter())
    assert result == "(1 * (2 + 3));\n"


def test_unary_operation():
    program = UnaryOperation("-", Number(42))
    result = program.accept(PrettyPrinter())
    assert result == "(-42);\n"


def test_function_call():
    program = FunctionCall(Reference("foo"), [Number(1),
                                              Number(2),
                                              Number(3)])
    result = program.accept(PrettyPrinter())
    assert result == "foo(1, 2, 3);\n"


def test_pretty_print(capsys):
    fcall = FunctionCall(Reference("foo"), [Reference("x")])
    cond = Conditional(BinaryOperation(Reference("x"), ">", Number(0)),
                       [fcall, Print(Reference("x"))],
                       [BinaryOperation(Reference("x"), "+", Number(121))])
    pretty_print(cond)
    out, err = capsys.readouterr()
    ans = """\
    if ((x > 0)) {
    \tfoo(x);
    \tprint x;
    } else {
    \t(x + 121);
    }\n"""
    assert out == dedent(ans)


def test_pretty_print_from_readme(capsys):
    pretty_print(FunctionDefinition("main", Function(["arg1"], [
        Read("x"),
        Print(Reference("x")),
        Conditional(
            BinaryOperation(Number(2), "==", Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference("exit"), [
                    UnaryOperation("-", Reference("arg1"))
                ])
            ],
        ),
    ])))
    out, err = capsys.readouterr()
    ans = """\
    def main(arg1) {
    \tread x;
    \tprint x;
    \tif ((2 == 3)) {
    \t\tif (1) {
    \t\t}
    \t} else {
    \t\texit((-arg1));
    \t}
    }\n"""
    assert out == dedent(ans)


def test_test_function_in_function_definition(capsys):
    f_def_1 = FunctionDefinition("foo_2", Function(["a", "b"],
                                                   [Print(Reference("a"))]))
    program = FunctionDefinition("foo", Function([], [f_def_1, Read("x")]))
    pretty_print(program)
    out, err = capsys.readouterr()

    ans = """\
    def foo() {
    \tdef foo_2(a, b) {
    \t\tprint a;
    \t}
    \tread x;
    }\n"""
    assert out == dedent(ans)


if __name__ == "__main__":
    pytest.main()
