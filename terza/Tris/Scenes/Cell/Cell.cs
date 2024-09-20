using System;
using Godot;
using Logic;

namespace Nodes;

public partial class Cell : Area2D
{
    [Export]
    public CellPosition TrisPosition = CellPosition.Top;

    [Export]
    public State BoardState;

    private Turn turn;
    public bool WasClicked { get; set; } = false;

    private bool gameIsOver = false;

    public override void _InputEvent(Viewport viewport, InputEvent @event, int shapeIdx)
    {
        if (BoardState.GameIsOver
            || WasClicked
            || !@event.IsActionPressed("mouse-left"))
        {
            return;
        }

        WasClicked = true;
        turn = BoardState.GetTurnAndCycle();

        var toShow = turn switch
        {
            Turn.CrossTurn => "Cross",
            Turn.CircTurn => "Circ",
            _ => throw new Exception("Invalid state")
        };

        BoardState.AddTurn(turn, TrisPosition);
        GetNode<Sprite2D>(toShow).Show();

        BoardState.CheckForWins(TrisPosition);

        if (BoardState.GameIsOver)
        {
            GetParent().GetParent<Board>().Winner = turn switch
            {
                Turn.CircTurn => GameWinner.Blue,
                Turn.CrossTurn => GameWinner.Red,
                _ => throw new Exception("impossible case")
            };
        }
        else if (BoardState.MovesCounter == 9)
        {
            BoardState.GameIsOver = true;
            GetParent().GetParent<Board>().Winner = GameWinner.None;
        }
    }
}
