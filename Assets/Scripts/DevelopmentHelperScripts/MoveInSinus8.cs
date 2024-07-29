using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using UnityEngine.XR.Management;

public class MoveInSinus8 : MonoBehaviour
{
    public float startPos;
    public float pos;
    public float scale;
    public float speed;
    public float speedGap;
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
        if (pos >= 4*Mathf.PI - stopTreshold && RoundDone)
        {
            Destroy(gameObject);
        }
        if (BullDozer)
            speed = 20;


        if (Time.time >= startPos)
        {
            automatic = true;
        }
        if (automatic) pos += Time.deltaTime * speed / scale * outsidefaster;
        outsidefaster = Mathf.Abs(Mathf.Sin(pos*SpeedzoneFrequency- (MoveSpeedzones* Mathf.PI))) + 1;  //try to get curves on outside as fast as in the middle of 8

        transform.position = sinFigure8(pos);
        vel = currentVel();
        // Look to the next point
        transform.LookAt(sinFigure8(pos + 0.02f));
    }

    float currentVel()
    {
        float s = (transform.position - lastPos).magnitude / Time.deltaTime;
        lastPos = transform.position;
        return s;
    }
    public Vector3 sinFigure8(float pos)
    {
        float x, y, z;

        y = transform.position.y;
        x = Mathf.Sin(pos) * scale;
        z = Mathf.Sin(pos * 2) * scale / Data.ScaleFactor;

        return new Vector3(x, y, z);
    }


}
