using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class toggle_with_button : MonoBehaviour
{
    public KeyCode toggle_key;

    List<GameObject> children;

    private void Start()
    {
        children = new List<GameObject>();
        for(int i = 0; i < transform.childCount; i++)
        {
            children.Add(transform.GetChild(i).gameObject);
        }
    }
    // Update is called once per frame
    void Update()
    {
        if (Input.GetKeyDown(toggle_key))
        {
            foreach(GameObject g in children)
            {
                g.SetActive(!g.activeSelf);
            }
        }
    }
}
