using Godot;
using System;

using BulletHell;

public class Restart : RestartFs
{
	[Signal]
	public delegate void RestartGameSameConfig();
}

