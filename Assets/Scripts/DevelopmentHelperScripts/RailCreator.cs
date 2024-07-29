using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using UnityEditor;
using static LinearEquation;

public class RailCreator : MonoBehaviour
{
    public int railRoundness = 4;
    public float resolution = 0.3f;
    public float width = 1 , thickness = 3;
    public Transform player;
    Vector3 lastPos;
    Vector3[] vert;
    int[] trian;
    Vector3[] lastCirclePoints;
    MeshFilter MF;
    public bool leftRail;
    public bool middle;
    public float middleStep;
    public Material middleMaterial;
    Vector3 lastMiddle;
    public float middlethickness;
    Mesh mesh;
    bool start, roundDone = false;
    float waitTime;
    public LinearEquation le;
    public readValues rv;
    float metersTraveled;

    // Start is called before the first frame update
    void Start()
    {
        player.position = v2tov3(f_polar(0));
        lastPos = v2tov3(f_polar(0 - 0.01f));
        vert = new Vector3[0];
        trian = new int[0];
        MF = GetComponent<MeshFilter>();
        lastCirclePoints = null;
        mesh = new Mesh();
    }

    // Update is called once per frame
    void Update()
    {
        waitTime += Time.deltaTime;



        if (le.currentTheta <= Mathf.PI*1.1f && !roundDone)
        {
            if (waitTime >= 5)
            {
                start = true;
                le.minSpeed = 5;
                le.maxSpeed = 5;
            }

            if (rv.metersTravelled - metersTraveled >= 1.5 && start)
            {
                if (Vector3.Distance(lastMiddle, player.position) > middleStep && middle)
                {
                    GameObject m = GameObject.CreatePrimitive(PrimitiveType.Cube);
                    m.GetComponent<MeshRenderer>().material = middleMaterial;
                    m.transform.parent = transform;
                    Destroy(m.GetComponent<Collider>());
                    m.transform.position = player.position;
                    m.transform.rotation = player.rotation;
                    m.transform.localScale = new Vector3(width * 2, middlethickness, middlethickness);
                    lastMiddle = player.position;
                }



                if (Vector3.Distance(lastPos, player.position) > resolution)
                {
                    Vector3 forward = player.position - lastPos;
                    Vector3 up = Vector3.Cross(forward, player.right).normalized * thickness;
                    Vector3 center = player.position + width * (leftRail ? -player.right : player.right);
                    Vector3[] circlePoints = new Vector3[railRoundness];

                    for (int i = 0; i < railRoundness; i++)
                    {
                        circlePoints[i] = Quaternion.AngleAxis(i * 360 / railRoundness, forward) * up + center;
                    }
                    if (player.GetComponent<LinearEquation>().currentTheta > Math.PI)
                    {
                        int oldLimit = trian.Length;
                        Array.Resize(ref trian, trian.Length + railRoundness * 2 * 3);

                        for (int i = 0; i < railRoundness; i++)
                        {
                            int l = vert.Length;
                            int start = oldLimit + i * 6;
                            trian[start + 0] = Array.IndexOf(vert, lastCirclePoints[i]);
                            trian[start + 1] = Array.IndexOf(vert, vert[(1 + i) % railRoundness]);
                            trian[start + 2] = Array.IndexOf(vert, vert[i]);
                            trian[start + 3] = Array.IndexOf(vert, lastCirclePoints[i]);
                            trian[start + 4] = Array.IndexOf(vert, lastCirclePoints[(i + 1) % railRoundness]);
                            trian[start + 5] = Array.IndexOf(vert, vert[(i + 1) % railRoundness]);
                        }
                        applyMesh();
#if UNITY_EDITOR
                        AssetDatabase.CreateAsset(mesh, "Assets/RailMesh.asset");
                        AssetDatabase.SaveAssets();
#endif

                        Destroy(this);
                    }


                    Array.Resize(ref vert, vert.Length + railRoundness);

                    for (int i = railRoundness; i > 0; i--)
                    {
                        vert[vert.Length - i] = circlePoints[railRoundness - i];
                    }


                    if (lastCirclePoints != null)
                    {
                        int oldLimit = trian.Length;
                        Array.Resize(ref trian, trian.Length + railRoundness * 2 * 3);

                        for (int i = 0; i < railRoundness; i++)
                        {
                            int start = oldLimit + i * 6;

                            bool[] last = new bool[]
                            {
                        true, false, false, true, true, false
                            };

                            int[] index = new int[]
                            {
                        0,1,0,0,1,1
                            };

                            for (int j = 0; j < 6; j++)
                            {

                                int ind = (index[j] + i) % railRoundness;

                                int indexOf = Array.IndexOf(vert, last[j] ? lastCirclePoints[ind] : circlePoints[ind]);

                                trian[start + j] = indexOf;

                            }

                        }
                    }
                    lastCirclePoints = circlePoints;

                    applyMesh();

                    lastPos = player.position;
                }

            }








        }
    }
    void applyMesh()
    {
        mesh.vertices = vert;
        mesh.triangles = trian;
        mesh.Optimize();
        mesh.RecalculateNormals();
        MF.mesh = mesh;


        }
}
