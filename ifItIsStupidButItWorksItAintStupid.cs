public class ifItIsStupidButItWorksItAintStupid : Godot.Node2D
{
    public override void _Ready()
    {
        this.GetTree().ChangeScene("res://MainScene.tscn");
    }
}

