using Ast;
using Ast.Declarations;
using Ast.Expressions;
using Ast.Programm;
using Ast.Statement;

using Runtime;

using ValueType = Runtime.ValueType;

namespace Execution;
public class AstEvaluator : IAstVisitor
{
    private readonly Context context;
    private readonly IEnvironment environment;
    private readonly Stack<Value> values = [];

    private readonly Stack<FunctionFrame> functionStack = new();
    private readonly Stack<LoopFrame> loopStack = new();

    public AstEvaluator(Context context, IEnvironment environment)
    {
        this.context = context;
        this.environment = environment;
    }

    public Value Evaluate(AstNode node)
    {
        if (values.Count > 0)
        {
            throw new InvalidOperationException(
                $"Evaluation stack must be empty, but contains {values.Count} values: {string.Join(", ", values)}"
            );
        }

        node.Accept(this);

        return values.Count switch
        {
            0 => throw new InvalidOperationException(
                "Evaluator logical error: the stack has no evaluation result"
            ),
            > 1 => throw new InvalidOperationException(
                $"Evaluator logical error: expected 1 value, got {values.Count} values: {string.Join(", ", values)}"
            ),
            _ => values.Pop(),
        };
    }

    public void Visit(ProgramNode p)
    {
        context.PushScope(new Scope());

        p.Module.Accept(this);

        foreach (ImportDecl import in p.Imports)
        {
            import.Accept(this);
        }

        foreach (AstNode node in p.TopLevelDecls)
        {
            node.Accept(this);
        }

        context.PopScope();

        values.Push(new Value(0));
    }

    public void Visit(FunctionCallExpression e)
    {
        List<Value> argsValues = new();
        foreach (Expression arg in e.Arguments)
        {
            arg.Accept(this);
            argsValues.Add(values.Pop());
        }

        if (EnvironmentFunctions.TryInvoke(e.Name, argsValues, environment, out Value? envResult))
        {
            values.Push(envResult!);
            return;
        }

        if (BuiltinFunctions.TryInvoke(e.Name, argsValues, out decimal builtInResult))
        {
            values.Push(
                builtInResult % 1 == 0
                    ? new Value((int)builtInResult)
                    : new Value(builtInResult)
            );
            return;
        }

        FunctionDecl func = context.GetFunction(e.Name);

        if (func.Parameters.Count != argsValues.Count)
        {
            throw new ArgumentException(
                $"Function {e.Name} expects {func.Parameters.Count} arguments, got {argsValues.Count}"
            );
        }

        context.PushScope(new Scope());
        functionStack.Push(new FunctionFrame());

        try
        {
            for (int i = 0; i < func.Parameters.Count; i++)
            {
                if (func.Parameters[i].ValueType != argsValues[i].GetValueType())
                {
                    throw new TypeErrorException(
                        $"Argument type mismatch for {func.Parameters[i].Name}"
                    );
                }

                context.DefineVariable(
                    func.Parameters[i].Name,
                    argsValues[i]
                );
            }

            func.Block.Accept(this);

            if (values.Count == 0)
            {
                throw new InvalidOperationException(
                    $"Function {func.Name} did not return a value"
                );
            }

            values.Push(values.Pop());
        }
        finally
        {
            functionStack.Pop();
            context.PopScope();
        }
    }

    public void Visit(FunctionDecl d)
    {
        context.DefineFunction(d);
    }

    public void Visit(ImportDecl d)
    {
        environment.AddImport(d);
    }

    public void Visit(ModuleDecl d)
    {
        environment.AddModule(d);
    }

    public void Visit(BlockStatement s)
    {
        context.PushScope(new Scope());
        foreach (Statement stmt in s.Statements)
        {
            stmt.Accept(this);

            if (functionStack.Count > 0 && functionStack.Peek().ReturnState)
            {
                break;
            }

            if (loopStack.Count > 0)
            {
                LoopFrame loop = loopStack.Peek();
                if (loop.Break || loop.Continue)
                {
                    break;
                }
            }
        }

        context.PopScope();
    }

    public void Visit(IfElseStatement s)
    {
        s.Condition.Accept(this);

        Value condition = values.Pop();

        if (condition.GetValueType() != Runtime.ValueType.Bool)
        {
            throw new TypeErrorException("Condition must be boolean");
        }

        if (condition.AsBool())
        {
            s.ThenBlock.Accept(this);
        }
        else if (s.ElseBlock is not null)
        {
            s.ElseBlock.Accept(this);
        }
    }

    public void Visit(AssignmentExpression e)
    {
        e.Value.Accept(this);
        Value value = values.Pop();

        Value variableValue = context.GetValue(e.Name);

        if (variableValue.GetValueType() != value.GetValueType())
        {
            throw new TypeErrorException($"Cannot assign {value.GetValueType()} to {variableValue.GetValueType()}");
        }

        context.AssignVariable(e.Name, value);

        values.Push(value);
    }

    public void Visit(ForLoopStatement e)
    {
        context.PushScope(new Scope());
        loopStack.Push(new LoopFrame());

        try
        {
            if (e.Init != null)
            {
                e.Init.Accept(this);
            }

            while (true)
            {
                if (e.Condition != null)
                {
                    e.Condition.Accept(this);
                    Value cond = values.Pop();

                    if (cond.GetValueType() != ValueType.Bool)
                    {
                        throw new TypeErrorException("Condition must be boolean");
                    }

                    if (!cond.AsBool())
                    {
                        break;
                    }
                }

                e.Block.Accept(this);

                LoopFrame loop = loopStack.Peek();

                if (loop.Break)
                {
                    break;
                }

                if (loop.Continue)
                {
                    loop.Continue = false;

                    if (e.Step != null)
                    {
                        e.Step.Accept(this);
                        values.Pop();
                    }
                }

                if (e.Step != null)
                {
                    e.Step.Accept(this);
                    values.Pop();
                }
            }
        }
        finally
        {
            loopStack.Pop();
            context.PopScope();
        }
    }

    public void Visit(WhileLoopStatement e)
    {
        loopStack.Push(new LoopFrame());

        try
        {
            while (true)
            {
                e.Condition.Accept(this);
                Value cond = values.Pop();

                if (cond.GetValueType() != ValueType.Bool)
                {
                    throw new TypeErrorException("Condition must be boolean");
                }

                if (!cond.AsBool())
                {
                    break;
                }

                e.Block.Accept(this);

                LoopFrame loop = loopStack.Peek();

                if (loop.Break)
                {
                    break;
                }

                if (loop.Continue)
                {
                    loop.Continue = false;
                }
            }
        }
        finally
        {
            loopStack.Pop();
        }
    }

    public void Visit(DoWhileLoopStatement s)
    {
        loopStack.Push(new LoopFrame());

        try
        {
            do
            {
                s.Block.Accept(this);

                LoopFrame loop = loopStack.Peek();

                if (loop.Break)
                {
                    break;
                }

                if (loop.Continue)
                {
                    loop.Continue = false;
                }

                s.Expression.Accept(this);
                Value cond = values.Pop();

                if (cond.GetValueType() != ValueType.Bool)
                {
                    throw new TypeErrorException("Condition must be boolean");
                }

                if (!cond.AsBool())
                {
                    break;
                }
            }
            while (true);
        }
        finally
        {
            loopStack.Pop();
        }
    }

    public void Visit(BreakStatement s)
    {
        if (loopStack.Count == 0)
        {
            throw new ArgumentException("'Break' can't be out of loop");
        }

        loopStack.Peek().Break = true;
    }

    public void Visit(ContinueStatement s)
    {
        if (loopStack.Count == 0)
        {
            throw new ArgumentException("'Continue' can't be out of loop");
        }

        loopStack.Peek().Continue = true;
    }

    public void Visit(ReturnStatement s)
    {
        if (functionStack.Count == 0)
        {
            throw new ArgumentException("'Return' can't be out of function");
        }

        FunctionFrame frame = functionStack.Peek();

        if (s.Value != null)
        {
            s.Value.Accept(this);
            Value returnValue = values.Pop();
            values.Push(returnValue);
        }

        frame.ReturnState = true;
    }

    public void Visit(LiteralExpression e)
    {
        values.Push(e.Value);
    }

    public void Visit(UnaryOperationExpression e)
    {
        e.Operand.Accept(this);
        Value value = values.Pop();
        switch (e.Operation)
        {
            case UnaryOperation.Plus:
                values.Push(value);
                break;
            case UnaryOperation.Minus:
                HandleUnaryMinus(value);
                break;
            case UnaryOperation.Not:
                HandleLogicalNot(value);
                break;
            case UnaryOperation.BitwiseNot:
                HandleBitsiweNot(value);
                break;
            default:
                throw new NotImplementedException($"Unknown unary operation {e.Operation}");
        }
    }

    public void Visit(ExpressionStatement s)
    {
        s.Expression.Accept(this);

        if (functionStack.Count > 0 && functionStack.Peek().ReturnState)
        {
            return;
        }

        Value val = values.Pop();
    }

    public void Visit(BinaryOperationExpression e)
    {
        e.Left.Accept(this);
        e.Right.Accept(this);
        Value right = values.Pop();
        Value left = values.Pop();

        switch (e.Operation)
        {
            case BinaryOperation.Add:
                HandleAdd(left, right);
                break;
            case BinaryOperation.Subtract:
                HandleSubtract(left, right);
                break;
            case BinaryOperation.Multiply:
                HandleMultiply(left, right);
                break;
            case BinaryOperation.Divide:
                HandleDivide(left, right);
                break;
            case BinaryOperation.Or:
                HandleLogicalOr(left, right);
                break;
            case BinaryOperation.And:
                HandleLogicalAnd(left, right);
                break;
            case BinaryOperation.Equal:
                values.Push(new Value(left.Equals(right)));
                break;
            case BinaryOperation.NotEqual:
                values.Push(new Value(!left.Equals(right)));
                break;
            case BinaryOperation.GreaterThan:
                HandleGreaterThan(left, right);
                break;
            case BinaryOperation.GreaterThanOrEqual:
                HandleGreaterThanOrEqual(left, right);
                break;
            case BinaryOperation.LessThan:
                HandleLessThan(left, right);
                break;
            case BinaryOperation.LessThanOrEqual:
                HandleLessThanOrEqual(left, right);
                break;
            case BinaryOperation.Module:
                HandleModule(left, right);
                break;
            case BinaryOperation.Exponent:
                HandleExponentiate(left, right);
                break;
            default:
                throw new NotImplementedException($"Unknown binary operation {e.Operation}");
        }
    }

    public void Visit(BitwiseOperationExpression e)
    {
        e.Left.Accept(this);
        e.Right.Accept(this);
        Value right = values.Pop();
        Value left = values.Pop();
        switch (e.Operation)
        {
            case BitwiseOperation.And:
                HandleAnd(left, right);
                break;
            case BitwiseOperation.Or:
                HandleOr(left, right);
                break;
            case BitwiseOperation.XOR:
                HandleXOR(left, right);
                break;
            default:
                throw new NotImplementedException($"Unknown bitwise operation {e.Operation}");
        }
    }

    public void Visit(VariableExpression e)
    {
        values.Push(context.GetValue(e.Name));
    }

    public void Visit(VariableDeclarationStatement s)
    {
        Value value;

        if (s.Type != null)
        {
            if (s.Value != null)
            {
                s.Value.Accept(this);
                value = values.Pop();

                if (value.GetValueType() != s.Type)
                {
                    throw new TypeErrorException($"Cannot assign value of type {value.GetValueType()} to let {s.Name}: {s.Type}");
                }
            }
            else
            {
                value = GetDefaultValue(s.Type.Value);
            }

            context.DefineConstant(s.Name, value);

            return;
        }

        if (s.Value == null)
        {
            throw new ArgumentException("Var cannot be without initializations");
        }

        s.Value.Accept(this);
        value = values.Pop();

        context.DefineVariable(s.Name, value);
    }

    private void HandleUnaryMinus(Value value)
    {
        if (value.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(-value.AsInt()));
            return;
        }
        else if (value.GetValueType() == ValueType.Float)
        {
            values.Push(new Value(-value.AsFloat()));
            return;
        }

        throw new TypeErrorException("Unary minus requires numeric type");
    }

    private void HandleLogicalNot(Value value)
    {
        if (value.GetValueType() != Runtime.ValueType.Bool)
        {
            throw new TypeErrorException("Logical NOT requires boolean type");
        }

        values.Push(new Value(!value.AsBool()));
    }

    private void HandleBitsiweNot(Value value)
    {
        if (value.GetValueType() != ValueType.Int)
        {
            throw new TypeErrorException("Bitwise NOT requires numeric type");
        }

        values.Push(new Value(~value.AsInt()));
    }

    private void HandleAdd(Value left, Value right)
    {
        ValueType leftType = left.GetValueType();
        ValueType rightType = right.GetValueType();

        switch ((leftType, rightType))
        {
            case (ValueType.Int, ValueType.Int):
                values.Push(new Value(left.AsInt() + right.AsInt()));
                break;
            case (ValueType.Float, ValueType.Float):
                values.Push(new Value(left.AsFloat() + right.AsFloat()));
                break;
            case (ValueType.String, ValueType.String):
                values.Push(new Value(left.AsString() + right.AsString()));
                break;
            default:
                throw new TypeErrorException("Unknown types");
        }
    }

    private void HandleSubtract(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() - right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            values.Push(new Value(left.AsFloat() - right.AsFloat()));
        }
        else
        {
            throw new TypeErrorException($"Cannot subtract types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleMultiply(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() * right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            values.Push(new Value(left.AsFloat() * right.AsFloat()));
        }
        else
        {
            throw new TypeErrorException($"Cannot multiply types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleDivide(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            if (right.AsInt() == 0)
            {
                throw new DivideByZeroException("Division by zero");
            }

            values.Push(new Value(left.AsInt() / right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            if (right.AsFloat() == 0)
            {
                throw new DivideByZeroException("Division by zero");
            }

            values.Push(new Value(left.AsFloat() / right.AsFloat()));
        }
        else
        {
            throw new TypeErrorException($"Cannot divide types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleModule(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            if (right.AsInt() == 0)
            {
                throw new DivideByZeroException("Modulo by zero");
            }

            values.Push(new Value(left.AsInt() % right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            if (right.AsFloat() == 0)
            {
                throw new DivideByZeroException("Modulo by zero");
            }

            values.Push(new Value(left.AsFloat() % right.AsFloat()));
        }
        else
        {
            throw new TypeErrorException($"Cannot modulo types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleLessThan(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() < right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            values.Push(new Value(left.AsFloat() < right.AsFloat()));
        }
        else if (right.GetValueType() == ValueType.String && left.GetValueType() == ValueType.String)
        {
            values.Push(new Value(string.Compare(left.AsString(), right.AsString(), StringComparison.Ordinal) < 0));
        }
        else
        {
            throw new TypeErrorException($"Cannot compare types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleGreaterThan(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() > right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            values.Push(new Value(left.AsFloat() > right.AsFloat()));
        }
        else if (right.GetValueType() == ValueType.String && left.GetValueType() == ValueType.String)
        {
            values.Push(new Value(string.Compare(left.AsString(), right.AsString(), StringComparison.Ordinal) > 0));
        }
        else
        {
            throw new TypeErrorException($"Cannot compare types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleLessThanOrEqual(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() <= right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            values.Push(new Value(left.AsFloat() <= right.AsFloat()));
        }
        else if (right.GetValueType() == ValueType.String && left.GetValueType() == ValueType.String)
        {
            values.Push(new Value(string.Compare(left.AsString(), right.AsString(), StringComparison.Ordinal) <= 0));
        }
        else
        {
            throw new TypeErrorException($"Cannot compare types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleGreaterThanOrEqual(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() >= right.AsInt()));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            values.Push(new Value(left.AsFloat() >= right.AsFloat()));
        }
        else if (right.GetValueType() == ValueType.String && left.GetValueType() == ValueType.String)
        {
            values.Push(new Value(string.Compare(left.AsString(), right.AsString(), StringComparison.Ordinal) >= 0));
        }
        else
        {
            throw new TypeErrorException($"Cannot compare types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleLogicalAnd(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Bool && left.GetValueType() == ValueType.Bool)
        {
            values.Push(new Value(left.AsBool() && right.AsBool()));
        }
        else
        {
            throw new TypeErrorException($"Logical AND requires boolean types, got {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleLogicalOr(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Bool && left.GetValueType() == ValueType.Bool)
        {
            values.Push(new Value(left.AsBool() || right.AsBool()));
        }
        else
        {
            throw new TypeErrorException($"Logical OR requires boolean types, got {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleExponentiate(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            double result = Math.Pow((double)left.AsInt(), (double)right.AsInt());
            values.Push(new Value((int)result));
        }
        else if (right.GetValueType() == ValueType.Float && left.GetValueType() == ValueType.Float)
        {
            double result = Math.Pow((double)left.AsFloat(), (double)right.AsFloat());
            values.Push(new Value((decimal)result));
        }
        else
        {
            throw new TypeErrorException($"Cannot exponentiate types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleAnd(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() & right.AsInt()));
        }
        else
        {
            throw new TypeErrorException($"Cannot bitwise and types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleOr(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() | right.AsInt()));
        }
        else
        {
            throw new TypeErrorException($"Cannot bitwise or types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleXOR(Value left, Value right)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() ^ right.AsInt()));
        }
        else
        {
            throw new TypeErrorException($"Cannot bitwise or types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private Value GetDefaultValue(ValueType type)
    {
        return type switch
        {
            ValueType.Int => new Value(0),
            ValueType.String => new Value("default"),
            ValueType.Bool => new Value(false),
            ValueType.Float => new Value(0),
            _ => throw new TypeErrorException("Unknown type"),
        };
    }

    private class FunctionFrame
    {
        public bool ReturnState { get; set; }
    }

    private class LoopFrame
    {
        public bool Break { get; set; }

        public bool Continue { get; set; }
    }
}
