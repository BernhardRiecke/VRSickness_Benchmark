using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TreeSpawner : MonoBehaviour
{
    public GameObject tree1, tree2, tree3, tree4;
    public bool spawnTree = false;
    List<GameObject> treeList;
    // Start is called before the first frame update
    void Start()
    {
        treeList = new List<GameObject>();
        treeList.Add(tree1);
        treeList.Add(tree2);
        treeList.Add(tree3);
        treeList.Add(tree4);
        spawnTree = true;
    }

    // Update is called once per frame
    void Update()
    {

        if (spawnTree)
        {
            TreeSpawning();
            spawnTree = false;
        }
    }

    void TreeSpawning()
    {
        for (int i = 0; i < 400; i++)
        {
            GameObject tree = treeList[Random.Range(0, 3)];
            tree = Instantiate(tree, new Vector3(Random.Range(-100, 100), 0f, Random.Range(68, -68)), new Quaternion(0, 0, 0, 0));
            tree.transform.eulerAngles = new Vector3(-90, 0, 0);
            float treeSize = Random.Range(0.5f, 1);
            tree.transform.localScale = new Vector3(treeSize,treeSize,treeSize);

        }
    }
}


