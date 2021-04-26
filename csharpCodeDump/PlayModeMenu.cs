using Godot;
using System;

using BulletHell;

public class PlayModeMenu : PlayModeMenuFs
{
    [Signal]
    public delegate void ChosenPlayMode(BulletHell.TimerType timer);
}

