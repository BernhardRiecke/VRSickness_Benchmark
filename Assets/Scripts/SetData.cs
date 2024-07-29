using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SetData : MonoBehaviour
{
    [Header("Fill before Start")]
    public string ID, age, gender;
    

    // Start is called before the first frame update
    void Start()
    {
        Data.name = ID;
        Data.age = age;
        Data.gender = gender;
    }

}
