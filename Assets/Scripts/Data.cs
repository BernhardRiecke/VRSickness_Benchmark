using System.Collections;
using System.Collections.Generic;
using UnityEngine;
public class Data
{
    public static string name ="testSubject", age="44", gender="male";
    public static int speedzoneFrequency  = 4;
    public static float scale = 3f, moveSpeedzones = 1.5f, ScaleFactor= 3f, minSpeed =0.1f, maxSpeed=2f,standStillTime = 0;
    public static List<float> SpeedPoints;
    public static bool ShowSpeedIndication=true;
    public static string envSize = "S", MSreductionMethod = "none", condition, locoMoInterface = "passive";
    public static NauseaScore.ControlMode translationMode = NauseaScore.ControlMode.passive, rotationMode = NauseaScore.ControlMode.passive;



}
