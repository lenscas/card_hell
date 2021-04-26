using Godot;
using System;

using BulletHell;

public class Node2D : Node2DFs
{
    [Signal]
    public delegate void End(int score);
}

