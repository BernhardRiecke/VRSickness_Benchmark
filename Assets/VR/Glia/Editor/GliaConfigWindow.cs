// (c) Copyright 2020-2021 HP Development Company, L.P.

using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;
using HP.Omnicept.Messaging.Messages;
using HP.Omnicept.Messaging;
using System;

namespace HP.Omnicept.Unity
{
    public class GliaConfigWindow : EditorWindow
    {
        protected GliaSettings m_settings;
        protected Editor m_settingsEditor;

        [MenuItem("HP Omnicept/Configure")]
        public static void CreateWindow()
        {
            GliaConfigWindow window = (GliaConfigWindow)EditorWindow.GetWindow(typeof(GliaConfigWindow), true);
            window.minSize = new Vector2(1000.0f, 400.0f);
        }

        private void OnEnable()
        {
            titleContent = new GUIContent("HP Omnicept");
            m_settings = Resources.Load("GliaSettings") as GliaSettings;

            if(m_settings == null)
            {
                m_settings = CreateInstance<GliaSettings>();
                if (!AssetDatabase.IsValidFolder("Assets/Resources"))
                {
                    AssetDatabase.CreateFolder("Assets", "Resources");
                }
                AssetDatabase.CreateAsset(m_settings, "Assets/Resources/GliaSettings.asset");
                AssetDatabase.SaveAssets();
                AssetDatabase.Refresh();
            }
        }

        private void OnDisable()
        {
            if(m_settingsEditor != null)
            {
                DestroyImmediate(m_settingsEditor);
            }
        }

        void OnGUI()
        {
            if (m_settingsEditor == null)
            {
                m_settingsEditor = Editor.CreateEditor(m_settings) as GliaSettingsEditor;
            }
            GUILayout.Space(20);
            m_settingsEditor.OnInspectorGUI();

            GUILayout.FlexibleSpace();
            
            EditorGUILayout.BeginHorizontal();
            GUILayout.FlexibleSpace();
            if(GUILayout.Button("Ok", GUILayout.MinWidth(150)))
            {
               Close();
            }
            GUILayout.FlexibleSpace();
            EditorGUILayout.EndHorizontal();
        }
    }

    [CustomEditor(typeof(GliaSettings))]
    public class GliaSettingsEditor : Editor
    {
        string clientIDField = "";
        string accessKeyField = "";
        GliaSettings.LicenseTypes licenseType;
        bool subscriptionFoldout = true;
public readonly List<uint> CORE_DISABLED_MESSAGES = new List<uint> { MessageTypes.ABI_MESSAGE_COGNITIVE_LOAD, MessageTypes.ABI_MESSAGE_HEART_RATE_VARIABILITY };
        public override void OnInspectorGUI()
        {
            GliaSettings gliaSettings = (GliaSettings) target;

            licenseType = (GliaSettings.LicenseTypes) EditorGUILayout.EnumPopup("LicenseType", gliaSettings.LicenseType);  

            if(licenseType == GliaSettings.LicenseTypes.Core){
                EditorGUILayout.BeginHorizontal();
                GUILayout.FlexibleSpace();
                EditorGUILayout.LabelField("Core does not have access to Cognitive Load and Heart Rate Variability", GUILayout.MinWidth(410));
                GUILayout.FlexibleSpace();
                EditorGUILayout.EndHorizontal();
            }   
            else{
                clientIDField = EditorGUILayout.PasswordField(new GUIContent("Client ID", gliaSettings.ClientID), gliaSettings.ClientID);
                accessKeyField = EditorGUILayout.PasswordField(new GUIContent("Access Key", gliaSettings.AccessKey), gliaSettings.AccessKey);
            }   

            GUILayout.Space(20);

            subscriptionFoldout = EditorGUILayout.BeginFoldoutHeaderGroup(subscriptionFoldout, "Subscriptions");
            EditorGUILayout.EndFoldoutHeaderGroup();

            if(subscriptionFoldout){
                Rect r = EditorGUILayout.BeginHorizontal();
                GUILayout.Space(10);
                EditorGUILayout.LabelField("Event", GUILayout.MinWidth(100));
                EditorGUILayout.LabelField("Sender", GUILayout.MinWidth(100));
                EditorGUILayout.LabelField("ID", GUILayout.MinWidth(100));
                EditorGUILayout.LabelField("Sub ID", GUILayout.MinWidth(100));
                EditorGUILayout.LabelField("Location", GUILayout.MinWidth(100));
                EditorGUILayout.LabelField("Version Semantic", GUILayout.MinWidth(100));
                GUILayout.Space(10);
                EditorGUILayout.EndHorizontal();
                foreach(var subItem in gliaSettings.UnitySubscriptions)
                {
bool isDisabled = !IsMessageEnabledOnlicense(subItem.m_messageType, licenseType);
                    EditorGUI.BeginDisabledGroup(isDisabled);
                    if (isDisabled)
                    {
                        subItem.m_enabled = false;
                    }
                    AddSubscriptionField(subItem);
                    EditorGUI.EndDisabledGroup();
                }
            }

            if (GUI.changed)
            {
                gliaSettings.ClientID = clientIDField.Trim();
                gliaSettings.AccessKey = accessKeyField.Trim();
                gliaSettings.LicenseType = licenseType;
                EditorUtility.SetDirty(target);
            }
        }

        private bool IsMessageEnabledOnlicense(uint m_messageType, GliaSettings.LicenseTypes licenseType)
        {
            if(licenseType ==  GliaSettings.LicenseTypes.Core){
                return !CORE_DISABLED_MESSAGES.Contains(m_messageType); 
            }

            return true;
        }

        public static void AddSubscriptionField(SubscriptionMenuItem menuItem)
        {
            Rect r = EditorGUILayout.BeginHorizontal();
            GUILayout.Space(10);
            menuItem.m_enabled = EditorGUILayout.ToggleLeft(menuItem.m_label, menuItem.m_enabled, GUILayout.MinWidth(100));
            menuItem.m_sender = EditorGUILayout.TextField(menuItem.m_sender, GUILayout.MinWidth(100));
            menuItem.m_id = EditorGUILayout.TextField(menuItem.m_id, GUILayout.MinWidth(100));
            menuItem.m_subid = EditorGUILayout.TextField(menuItem.m_subid, GUILayout.MinWidth(100));
            menuItem.m_location = EditorGUILayout.TextField(menuItem.m_location, GUILayout.MinWidth(100));
            menuItem.m_versionSemantic = EditorGUILayout.TextField(menuItem.m_versionSemantic, GUILayout.MinWidth(100));
            GUILayout.Space(10);
            EditorGUILayout.EndHorizontal();
        }

    }
}
