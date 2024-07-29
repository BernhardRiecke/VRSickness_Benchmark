using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Speed : MonoBehaviour
{
    public float speed;

    Vector3 lastPos;

    private void Start()
    {
        lastPos = transform.position;
    }
    // Update is called once per frame
    void Update()
    {
        speed = (transform.position - lastPos).magnitude / Time.deltaTime;
        lastPos = transform.position;
    }
}
