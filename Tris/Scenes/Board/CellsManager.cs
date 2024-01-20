using Godot;
using Nodes;

namespace Logic;

public partial class CellsManager : Node2D
{
    public override void _Ready()
    {
        foreach (var child in GetChildren())
        {
            var cell = child as Cell;

            (float x, float y) = (cell.Position.X, cell.Position.Y);
            float offSet = 110;

            Vector2 newPosition = cell.TrisPosition switch
            {
                CellPosition.TopLeft => new Vector2(x - offSet, y - offSet),
                CellPosition.Top => new Vector2(x, y - offSet),
                CellPosition.TopRight => new Vector2(x + offSet, y - offSet),

                CellPosition.MiddleLeft => new Vector2(x - offSet, y),
                CellPosition.Middle => cell.Position,
                CellPosition.MiddleRight => new Vector2(x + offSet, y),

                CellPosition.BottomLeft => new Vector2(x - offSet, y + offSet),
                CellPosition.Bottom => new Vector2(x, y + offSet),
                CellPosition.BottomRight => new Vector2(x + offSet, y + offSet),

                _ => Vector2.Zero
            };

            cell.Position = newPosition;
        }
    }

    public void HideAllCells()
    {
        foreach (var child in GetChildren())
        {
            if (child is Cell cell)
            {
                cell.WasClicked = false;
                cell.GetNode<Sprite2D>("Circ").Hide();
                cell.GetNode<Sprite2D>("Cross").Hide();
            }
        }
    }
}
