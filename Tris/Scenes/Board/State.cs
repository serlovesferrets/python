using System;
using Godot;

namespace Logic;

public enum Turn
{
    CrossTurn,
    CircTurn
}

public enum CellState
{
    Empty,
    Circled,
    Crossed
}

public partial class State : Node
{
    [Export]
    public Turn Turn = Turn.CrossTurn;

    CellState[,] playsMatrix = new CellState[,] {
        { CellState.Empty, CellState.Empty, CellState.Empty },
        { CellState.Empty, CellState.Empty, CellState.Empty },
        { CellState.Empty, CellState.Empty, CellState.Empty },
    };

    public int MovesCounter { get; set; } = 0;

    public bool GameIsOver = false;

    public static (int, int) MatrixIdxFromCell(CellPosition position) =>
        position switch
        {
            CellPosition.TopLeft => (0, 0),
            CellPosition.Top => (0, 1),
            CellPosition.TopRight => (0, 2),

            CellPosition.MiddleLeft => (1, 0),
            CellPosition.Middle => (1, 1),
            CellPosition.MiddleRight => (1, 2),

            CellPosition.BottomLeft => (2, 0),
            CellPosition.Bottom => (2, 1),
            CellPosition.BottomRight => (2, 2),

            _ => throw new Exception("Invalid state")
        };

    public CellState GetMatrixAt(CellPosition position)
    {
        var (x, y) = MatrixIdxFromCell(position);
        return playsMatrix[x, y];
    }

    public void AddTurn(Turn turn, CellPosition cell)
    {
        var (x, y) = MatrixIdxFromCell(cell);

        var turnToSet = turn == Turn.CircTurn
            ? CellState.Circled
            : CellState.Crossed;

        playsMatrix[x, y] = turnToSet;
    }

    public void CheckForWins(CellPosition lastTurn)
    {
        var current = GetMatrixAt(lastTurn);

        var c = current;
        var GMA = GetMatrixAt;

        GameIsOver = lastTurn switch
        {
            CellPosition.TopLeft =>
                (c == GMA(CellPosition.Top) && c == GMA(CellPosition.TopRight))
                || (c == GMA(CellPosition.MiddleLeft) && c == GMA(CellPosition.BottomLeft))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.BottomRight)),
            CellPosition.Top =>
                (c == GMA(CellPosition.TopLeft) && c == GMA(CellPosition.TopRight))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.Bottom)),
            CellPosition.TopRight =>
                (c == GMA(CellPosition.Top) && c == GMA(CellPosition.TopLeft))
                || (c == GMA(CellPosition.MiddleRight) && c == GMA(CellPosition.BottomRight))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.BottomLeft)),

            CellPosition.MiddleLeft =>
                (c == GMA(CellPosition.TopLeft) && c == GMA(CellPosition.BottomLeft))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.MiddleLeft)),
            CellPosition.Middle =>
                (c == GMA(CellPosition.Top) && c == GMA(CellPosition.Bottom))
                || (c == GMA(CellPosition.MiddleLeft) && c == GMA(CellPosition.MiddleRight))
                || (c == GMA(CellPosition.TopLeft) && c == GMA(CellPosition.BottomRight))
                || (c == GMA(CellPosition.TopRight) && c == GMA(CellPosition.BottomLeft)),
            CellPosition.MiddleRight =>
                (c == GMA(CellPosition.TopRight) && c == GMA(CellPosition.BottomRight))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.MiddleLeft)),

            CellPosition.BottomLeft =>
                (c == GMA(CellPosition.Bottom) && c == GMA(CellPosition.BottomRight))
                || (c == GMA(CellPosition.MiddleLeft) && c == GMA(CellPosition.TopLeft))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.TopRight)),
            CellPosition.Bottom =>
                (c == GMA(CellPosition.BottomLeft) && c == GMA(CellPosition.BottomRight))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.Top)),
            CellPosition.BottomRight =>
                (c == GMA(CellPosition.Bottom) && c == GMA(CellPosition.BottomLeft))
                || (c == GMA(CellPosition.MiddleRight) && c == GMA(CellPosition.TopRight))
                || (c == GMA(CellPosition.Middle) && c == GMA(CellPosition.TopLeft)),

            _ => false
        };
    }

    public void UpdateTurn()
    {
        Turn = Turn switch
        {
            Turn.CrossTurn => Turn.CircTurn,
            Turn.CircTurn => Turn.CrossTurn,
            _ => throw new Exception("Invalid state")
        };
        MovesCounter++;
    }

    public Turn GetTurnAndCycle()
    {
        var turn = Turn;
        UpdateTurn();
        return turn;
    }

    public void ResetState()
    {
        playsMatrix = new CellState[,] {
            { CellState.Empty, CellState.Empty, CellState.Empty },
            { CellState.Empty, CellState.Empty, CellState.Empty },
            { CellState.Empty, CellState.Empty, CellState.Empty },
        };

        Turn = Turn.CrossTurn;
        GameIsOver = false;
        MovesCounter = 0;
        GetParent().GetNode<CellsManager>("Cells").HideAllCells();
        GetParent<Board>().Winner = GameWinner.GameNotOver;
    }
}
