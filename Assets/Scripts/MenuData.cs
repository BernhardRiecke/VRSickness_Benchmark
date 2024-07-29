using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;
using TMPro;
using UnityEngine.XR.Management;

public class MenuData : MonoBehaviour
{
    public Slider scaleSlider, SpeedSlider, speedzoneFrequencySlider, StandStillTimeSlider, MoveSpeedZonesSlider;
    public Toggle showSpeedIndication;
    public TextMeshProUGUI scaleText, speedText, speedzoneFrequencyText, StandStillText, MoveSpeedZonesText;
    public DrawRoad road;
    public GameObject cam;
    int previewCamHeight = 270;
    float pos, outsidefaster, velocity=0;
    bool velDown = true;
    public GameObject RedMarker, GreenMarker;
    List<GameObject> MarkerList;
    List<float> SpeedPointList;
    bool computeSZ=true;

    private void Start()
    {
        //deactivate VR in MainMenu
        XRGeneralSettings.Instance.Manager.DeinitializeLoader();
        MarkerList = new List<GameObject>();
        SpeedPointList = new List<float>();
        getSpeed();
        getSize();
        getMoveSpeedZones();
        getSpeedZoneFrequency();
        getStandStillTime();
        setSpeedIndication();
    }

    private void Update()
    {
        if (!Input.GetMouseButton(0) && computeSZ == true)
        {
            StopAllCoroutines();
            StartCoroutine(computeSpeedzones());
            computeSZ = false;
        }
    }
    public void getSpeed()
    {
        Data.maxSpeed = SpeedSlider.value /10f;
        speedText.text = "max. Speed " + Data.maxSpeed + " m/s";
    }
    public void getSize()
    {
        Data.scale = scaleSlider.value/10f * 0.263f;
        scaleText.text = "Length " + scaleSlider.value/10f + " m";
        road.PreComputeRoad();
        cam.transform.position = new Vector3(0, previewCamHeight + Data.scale, 0);
        computeSZ = true;
        clearList();
    }
    public void getStandStillTime()
    {
        Data.standStillTime = StandStillTimeSlider.value;
        StandStillText.text = "Stand Still Time " + Data.standStillTime + " s";
    }

    public void getSpeedZoneFrequency()
    {
        Data.speedzoneFrequency = (int)speedzoneFrequencySlider.value*2;
        speedzoneFrequencyText.text = "Speedzone Frequency " + Data.speedzoneFrequency;
        computeSZ = true;
        clearList();
    }
    public void getMoveSpeedZones()
    {
        Data.moveSpeedzones = MoveSpeedZonesSlider.value+0.5f;
        MoveSpeedZonesText.text = "Move Speedzones " + MoveSpeedZonesSlider.value;
        computeSZ = true;
        clearList();
    }
    public void setSpeedIndication()
    {
        Data.ShowSpeedIndication = showSpeedIndication.isOn;
    }
    public void loadlevel()
    {
        transferSpeedPoints();
        SceneManager.LoadScene("StartScene");
    }
    IEnumerator computeSpeedzones()
    {
        //Ensure that only one Coroutine is Running
        StopCoroutine(computeSpeedzones());

        //Reset old variables
        float oldOutsidefaster, lastPos = 0;
        outsidefaster = 1;
        velDown = Data.moveSpeedzones < 1;
        

        yield return new WaitForSeconds(0.1f);
        //Compute the Speedzones
        for (pos = 0; pos < 2 * Mathf.PI; pos += 0.01f)
        {
            oldOutsidefaster = outsidefaster;
            outsidefaster = Mathf.Sin(pos * Data.speedzoneFrequency - (Data.moveSpeedzones * Mathf.PI));  //try to get curves on outside as fast as in the middle of 8


            if (outsidefaster > oldOutsidefaster)
            {
                if (velDown)
                {
                    //Debug.Log("Tiefpunkt bei:" + sinFigure8(lastPos));
                    MarkerList.Add(Instantiate(RedMarker, sinFigure8(lastPos) + road.transform.position, new Quaternion(0, 0, 0, 0)));
                    SpeedPointList.Add(lastPos);
                    velDown = false;
                }

            }
            else if (!velDown)
            {
                velDown = true;
                //Debug.Log("Hochpunkt bei:" + sinFigure8(lastPos));
                MarkerList.Add(Instantiate(GreenMarker, sinFigure8(lastPos) + road.transform.position, new Quaternion(0, 0, 0, 0)));
                SpeedPointList.Add(lastPos);
            }
            lastPos = pos;
        }

    }

    //Compute SinusFigure8
    Vector3 sinFigure8(float pos)
    {
        float x, y, z;

        y = transform.position.y;
        x = Mathf.Sin(pos) * Data.scale;
        z = Mathf.Sin(pos * 2) * Data.scale / Data.ScaleFactor;

        return new Vector3(x, y, z);
    }

    // Clear List of Speedzonepoints
    public void clearList()
    {
        foreach (var item in MarkerList)
        {
            Destroy(item);
        }
        MarkerList.Clear();
        SpeedPointList.Clear();
    }
    void transferSpeedPoints()
    {
        Data.SpeedPoints = new List<float>();
        foreach (var item in SpeedPointList)
        {
            Data.SpeedPoints.Add(item);
        }
    }
}
