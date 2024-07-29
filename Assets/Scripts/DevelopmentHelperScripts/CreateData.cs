using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

public static class CreateData
{
    static string root;
    static public float timeElapsed;

    public static IEnumerator CreateCSV(string DataName, string Title, float Data)
    {
        timeElapsed += Time.fixedDeltaTime;

        root = Application.persistentDataPath + "/" + DataName + ".csv";
        Debug.Log(root);


        if (!File.Exists(root))
        {

            var sr = File.CreateText(root);
            string dataCSV = "Time;" + Title + ";";
            sr.WriteLine(dataCSV);

            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;


            sr.Close();

        }

        else
        {
            var d = Data;
            string dataCSV = timeElapsed.ToString("0.00") + ";" + d.ToString();
            dataCSV += System.Environment.NewLine;

            dataCSV = dataCSV.Replace('.', ',');

            File.AppendAllText(root, dataCSV);
            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;




        }
        yield return new WaitForSeconds(2f);
    }
    public static IEnumerator CreateCSV(string DataName, string Title, int Data)
    {
        timeElapsed += Time.fixedDeltaTime;

        root = Application.persistentDataPath + "/" + DataName + ".csv";
        Debug.Log(root);


        if (!File.Exists(root))
        {

            var sr = File.CreateText(root);
            string dataCSV = "Time;" + Title + ";";

            sr.WriteLine(dataCSV);

            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;


            sr.Close();

        }

        else
        {
            var d = Data;
            string dataCSV = timeElapsed.ToString("0.00") + ";" + d.ToString();

            dataCSV += System.Environment.NewLine;

            dataCSV = dataCSV.Replace('.', ',');

            File.AppendAllText(root, dataCSV);
            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;





        }
        yield return new WaitForSeconds(2f);
    }
    public static IEnumerator CreateCSV(string DataName, string Title, string Data)
    {
        timeElapsed += Time.fixedDeltaTime;

        root = Application.persistentDataPath + "/" + DataName + ".csv";
        Debug.Log(root);

        if (!File.Exists(root))
        {

            var sr = File.CreateText(root);
            string dataCSV = "Time;" + Title + ";";
            sr.WriteLine(dataCSV);

            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;


            sr.Close();

        }

        else
        {
            var d = Data;
            string dataCSV = timeElapsed.ToString("0.00") + ";" + d.ToString();
            dataCSV += System.Environment.NewLine;

            dataCSV = dataCSV.Replace('.', ',');

            File.AppendAllText(root, dataCSV);
            FileInfo fInfo = new FileInfo(root);
            fInfo.IsReadOnly = false;





        }
        yield return new WaitForSeconds(2f);
    }
}
