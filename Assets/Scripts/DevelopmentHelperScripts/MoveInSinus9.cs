using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using UnityEngine.XR.Management;

public class MoveInSinus9 : MonoBehaviour
{
    public float startPos;
    public float pos;
    public float scale;
    public float speed, minSpeed, maxSpeed;
    public float speedGap;
    public float time;
    public float x, y, z;
    [Range(0.5f, 1.0f)]
    public float MoveSpeedzones;
    [Range(1, 10)]
    public int SpeedzoneFrequency;
    bool automatic;
    public bool RoundDone =false;
    public float stopTreshold;
    public float vel;
    public bool BullDozer;

    TextMeshProUGUI velocity;
    public float outsidefaster;

    Vector3 lastPos;
    private void Awake()
    {
        XRGeneralSettings.Instance.Manager.InitializeLoaderSync();
        XRGeneralSettings.Instance.Manager.StartSubsystems();

        //Get Data for Player Movement Script from mainmenu
        speed = Data.maxSpeed;
        scale = Data.scale;
        SpeedzoneFrequency = Data.speedzoneFrequency;
        MoveSpeedzones = Data.moveSpeedzones;
    }
    private void Start()
    {

        automatic = false;
        lastPos = transform.position;
        velocity = GetComponentInChildren<TextMeshProUGUI>();
    }
    void Update()
    {
        time += Time.deltaTime;
        speedCompute();
        transform.position = sinFigure8();
    }

    float currentVel()
    {
        float s = (transform.position - lastPos).magnitude / Time.deltaTime;
        lastPos = transform.position;
        return s;
    }

    void speedCompute()
    {
       speed =  Mathf.Sin(time) * (maxSpeed-minSpeed) + minSpeed;
    }

    public Vector3 sinFigure8()
    {
        Debug.Log(time % 12);
        y = transform.position.y;
        x = Mathf.Sin(time * (2 * Mathf.PI / 12)); // (minSpeed + maxSpeed / 2) * speed;
        z = Mathf.Sin(time * (Mathf.PI/12)); // (minSpeed + maxSpeed / 2) * speed;
      //  speed = ((Mathf.Sin(Time.realtimeSinceStartup * ((2 * Mathf.PI) / 19.5f)) + 1) / 2) * 7

        return new Vector3(x, y, z);
    }


}
