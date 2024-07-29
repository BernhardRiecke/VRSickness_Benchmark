using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

using UnityEngine.SceneManagement;


public class mainMenu : MonoBehaviour
{
    public Button smallEnv;
    public Button mediumEnv;
    public Button largeEnv;

    void Start()
    {
        Button sebtn = smallEnv.GetComponent<Button>();
        Button mebtn = mediumEnv.GetComponent<Button>();
        Button lebtn = largeEnv.GetComponent<Button>();

        sebtn.onClick.AddListener(SEOnClick);
        mebtn.onClick.AddListener(MEOnClick);
        lebtn.onClick.AddListener(LEOnClick);

    }

    void SEOnClick()
    {
        SceneManager.LoadScene("Small Scene", LoadSceneMode.Single);
    }

    void MEOnClick()
    {
        SceneManager.LoadScene("Medium Scene", LoadSceneMode.Single);
    }

    void LEOnClick()
    {
        SceneManager.LoadScene("Large Scene", LoadSceneMode.Single);
    }
}
