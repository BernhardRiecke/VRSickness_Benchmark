using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DrawRoad : MonoBehaviour
{
    public float samplingRate = 0.1f;
    public Vector3[] positionArray;
    float x, y, z;


    // Start is called before the first frame update
    void Start()
    {
        PreComputeRoad();

        /*
        LineRenderer lineRenderer = GetComponent<LineRenderer>();
        MeshCollider meshCollider = transform.gameObject.AddComponent<MeshCollider>();

        Mesh mesh = new Mesh();
        lineRenderer.BakeMesh(mesh, true);
        meshCollider.sharedMesh = mesh;
        */
    }

    public void PreComputeRoad()
    {
        //Get Size for PositionArray
        int j = (int)((Mathf.PI * 2) / samplingRate) + 1;
        positionArray = new Vector3[j];

       //Compute Sinus8
        for (int i = 0; i < j; i++)
        {
            y = transform.position.y;
            x = Mathf.Sin(i * samplingRate) * Data.scale;
            z = Mathf.Sin(i * samplingRate * 2) * Data.scale / Data.ScaleFactor;
            positionArray[i] = new Vector3(x, y, z);
        }
        
        //Fill LineRenderer
        GetComponent<LineRenderer>().positionCount = positionArray.Length;
        GetComponent<LineRenderer>().SetPositions(positionArray);
    }
}

