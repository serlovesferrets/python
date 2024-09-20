using Godot;
using Logic;

public enum GameWinner
{
    GameNotOver,
    Red,
    Blue,
    None
}

public partial class Board : Node2D
{
    GameWinner _winner = GameWinner.GameNotOver;
    public GameWinner Winner
    {
        get => _winner;
        set
        {
            var outText = value switch
            {
                GameWinner.Red => "Red won!!",
                GameWinner.Blue => "Blue won!!",
                GameWinner.None => "It's a draw!",
                GameWinner.GameNotOver => "Game in progress",
                _ => "huh"
            };

            GetParent().GetNode<Label>("WinnerText").Text = outText;

            _winner = value;
        }
    }

    public override void _Ready()
    {
        var btn = GetTree().GetFirstNodeInGroup("Buttons") as Button;
        btn.Pressed += GetNode<State>("State").ResetState;
    }
}
