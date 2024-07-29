using UnityEngine;
using UnityEditor;

[CustomEditor(typeof(NauseaScore))]
public class VignetteSize : Editor
{
    public override void OnInspectorGUI()
    {
        base.OnInspectorGUI();
        NauseaScore nausea = (NauseaScore)target;
            nausea.ChangeVignette();


    }
}
