using Ast;
using Ast.Expressions;
using Ast.Statement;

using Runtime;

using ValueType = Runtime.ValueType;

namespace Execution;
public class AstEvaluator : IAstVisitor
{
    private readonly Context context;
    private readonly IEnvironment environment;
    private readonly Stack<Value> values = [];

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

    public void Visit(AssignmentStatement e)
    {
        e.Value.Accept(this);
        Value value = values.Peek();
        if (context.GetValue(e.Name).GetValueType() != value.GetValueType())
        {
            throw new TypeErrorException("Unknown type");
        }

        context.AssignVariable(e.Name, value);
    }

    public void Visit(BlockStatement e)
    {

    }

    public void Visit(IfElseStatement e)
    {
        e.Condition.Accept(this);

        Value condition = values.Pop();


        if (condition.GetValueType() != Runtime.ValueType.Bool)
        {
            throw new TypeErrorException("Condition must be boolean");
        }

        if (condition.AsBool())
        {
            e.ThenBlock.Accept(this);
        }
        else if (e.ElseBlock is not null)
        {
            e.ElseBlock.Accept(this);
        }
    }

    public void Visit(ForLoopStatement e)
    {
        context.PushScope(new Scope());

    }

    public void Visit(WhileLoopStatement e)
    {
        context.PushScope(new Scope { LoopScope = true });

        Scope loopScope = context.GetLastScope();

        while (true)
        {
            e.Condition.Accept(this);
            Value condition = values.Pop();
            if (condition.GetValueType() != Runtime.ValueType.Bool)
            {
                throw new TypeErrorException("Condition must be boolean");
            }

            if (!condition.AsBool())
            {
                break;
            }

            e.Block.Accept(this);

            if (loopScope.ReturnState)
            {
                break;
            }

            if (loopScope.BreakState)
            {
                break;
            }

            if (loopScope.ContinueState)
            {
                loopScope.ContinueState = false;
                continue;
            }
        }

        context.PopScope();
    }

    public void Visit(DoWhileLoopStatement s)
    {
        context.PushScope(new Scope { LoopScope = true });
        Scope loopScope = context.GetLastScope();

        do
        {
            s.Block.Accept(this);

            if (loopScope.ReturnState)
            {
                break;
            }

            if (loopScope.ContinueState)
            {
                loopScope.ContinueState = false;
            }

            s.Expression.Accept(this);
            Value condition = values.Pop();

            if (condition.GetValueType() != Runtime.ValueType.Bool)
            {
                throw new TypeErrorException("Condition must be boolean");
            }

            if (!condition.AsBool())
            {
                break;
            }

        } while (true);

        context.PopScope();
    }

    public void Visit(BreakStatement s)
    {
        Scope latsScope = context.GetLastScope();
        if (!latsScope.LoopScope)
        {
            throw new ArgumentException("'Continue' can't be out of loop");
        }

        latsScope.BreakState = true;
    }

    public void Visit(ContinueStatement s)
    {
        Scope latsScope = context.GetLastScope();
        if (!latsScope.LoopScope)
        {
            throw new ArgumentException("'Continue' can't be out of loop");
        }

        latsScope.ContinueState = true;
    }

    public void Visit(ReturnStatement s)
    {
        Scope scope = context.GetLastScope();

        if (!scope.FuncScope)
        {
            throw new ArgumentException("'Return' can't be out of function");
        }

        if (s.Value is not null)
        {
            s.Value.Accept(this);
            if (values.Peek().GetValueType() != s.Type || s.Type == Runtime.ValueType.Void)
            {
                throw new TypeErrorException("Unknown types");
            }
        }
        else if (s.Type != Runtime.ValueType.Void)
        {
            throw new TypeErrorException("Unknown types");
        }

        scope.ReturnState = true;
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
        s.Value.Accept(this);
        Value value = values.Pop();

        context.DefineVariable(s.Name, value);
    }

    public void Visit(LetDeclarationStatement s)
    {
        Value value;

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
            value = GetDefaultValue(s.Type);
        }

        context.DefineConstant(s.Name, value);
    }

    private void HandleUnaryMinus(Value value)
    {
        if (value.GetValueType() != Runtime.ValueType.Int)
        {
            throw new TypeErrorException("Unary minus requires numeric type");
        }

        values.Push(new Value(-value.AsInt()));
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
            case (ValueType.String, ValueType.String):
                values.Push(new Value(left.AsString() + right.AsString()));
                break;
            default:
                throw new TypeErrorException("Unknown types");
        }
    }

    private void HandleSubtract(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() - right.AsInt()));
        }
        else
        {
            throw new TypeErrorException($"Cannot subtract types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleMultiply(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() * right.AsInt()));
        }
        else
        {
            throw new TypeErrorException($"Cannot multiply types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleDivide(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            if (right.AsInt() == 0)
            {
                throw new DivideByZeroException("Division by zero");
            }

            values.Push(new Value(left.AsInt() / right.AsInt()));
        }
        else
        {
            throw new TypeErrorException($"Cannot divide types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleModule(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            if (right.AsInt() == 0)
            {
                throw new DivideByZeroException("Modulo by zero");
            }

            values.Push(new Value(left.AsInt() % right.AsInt()));
        }
        else
        {
            throw new TypeErrorException($"Cannot modulo types {left.GetValueType()} and {right.GetValueType()}");
        }
    }

    private void HandleLessThan(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() < right.AsInt()));
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

    private void HandleGreaterThan(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() > right.AsInt()));
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

    private void HandleLessThanOrEqual(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() <= right.AsInt()));
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

    private void HandleGreaterThanOrEqual(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            values.Push(new Value(left.AsInt() >= right.AsInt()));
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

    private void HandleLogicalAnd(Value right, Value left)
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

    private void HandleLogicalOr(Value right, Value left)
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

    private void HandleExponentiate(Value right, Value left)
    {
        if (right.GetValueType() == ValueType.Int && left.GetValueType() == ValueType.Int)
        {
            double result = Math.Pow((double)left.AsInt(), (double)right.AsInt());
            values.Push(new Value((int)result));
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
            _ => throw new TypeErrorException("Unknown type"),
        };
    }
}
