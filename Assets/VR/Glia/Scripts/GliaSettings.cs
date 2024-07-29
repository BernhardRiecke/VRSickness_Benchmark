// (c) Copyright 2020-2021 HP Development Company, L.P.

using System;
using System.Text;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.IO;
using UnityEngine;
using HP.Omnicept.Messaging.Messages;

namespace HP.Omnicept.Unity
{

    [System.Serializable]
    public class SubscriptionMenuItem
    {
        [SerializeField]
        public string m_label;
        [SerializeField]
        public bool m_enabled;
        [SerializeField]
        public uint m_messageType;
        [SerializeField]
        public string m_sender = "";
        [SerializeField]
        public string m_id = "";
        [SerializeField]
        public string m_subid = "";
        [SerializeField]
        public string m_location = "";
        [SerializeField]
        public string m_versionSemantic = "";

        public SubscriptionMenuItem(uint messageType, string label)
        {
            m_messageType = messageType;
            m_label = label;
            m_enabled = true;
        }
    }

    public class GliaSettings : ScriptableObject
    {
        public enum LicenseTypes
        {
            Core = LicensingModel.Core,
            Developer = LicensingModel.Rev_Share
        }

        public LicenseTypes LicenseType = LicenseTypes.Developer;

        [HideInInspector]
        public LicensingModel RequestedLicense => (LicensingModel) LicenseType;

        [HideInInspector]
        [SerializeField]
        private byte[] ClientIDBytes;

        [HideInInspector]
        [SerializeField]
        private byte[] AccessKeyBytes;

        [HideInInspector]
        public SubscriptionList SubList = new SubscriptionList();

        private byte[] ENCRYPT_KEY = Encoding.ASCII.GetBytes("1CaIy2QAi27pyQvZcTGaGLbDxDGzBxZP");
        private byte[] ENCRYPT_IV = Encoding.ASCII.GetBytes("vq8J6AQOe1UjX22u");

        public string ClientID
        {
            get
            {
                string id = "";
                if (ClientIDBytes?.Length > 0)
                {
                    id = DecryptStringFromBytes_Aes(ClientIDBytes, ENCRYPT_KEY, ENCRYPT_IV);
                }
                return id;
            }

            set
            {
                if (!String.IsNullOrEmpty(value))
                {
                    ClientIDBytes = EncryptStringToBytes_Aes(value, ENCRYPT_KEY, ENCRYPT_IV);
                }
                else
                {
                    ClientIDBytes = new byte[] { };
                }
            }
        }

        public string AccessKey
        {
            get
            {
                string key = "";
                if (AccessKeyBytes?.Length > 0)
                {
                    key = DecryptStringFromBytes_Aes(AccessKeyBytes, ENCRYPT_KEY, ENCRYPT_IV);
                }
                return key;
            }

            set
            {
                if (!String.IsNullOrEmpty(value))
                {
                    AccessKeyBytes = EncryptStringToBytes_Aes(value, ENCRYPT_KEY, ENCRYPT_IV);
                }
                else
                {
                    AccessKeyBytes = new byte[] { };
                }
            }
        }

        [HideInInspector]
        [SerializeField]
        public List<SubscriptionMenuItem> UnitySubscriptions = new List<SubscriptionMenuItem>()
        {
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_COGNITIVE_LOAD, "Cognitive Load"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_HEART_RATE, "Heart Rate"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_HEART_RATE_VARIABILITY, "Heart Rate Variability"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_EYE_TRACKING, "Eye Tracking"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_CAMERA_IMAGE, "Camera Image"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_IMU_FRAME, "IMU"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_PPG_FRAME, "PPG"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_VSYNC, "VSync"),
            new SubscriptionMenuItem(MessageTypes.ABI_MESSAGE_SUBSCRIPTION_RESULT_LIST, "Subscription Result List")
        };

        public SubscriptionList GetSubscriptionList()
        {
            SubList.Subscriptions.Clear();
            foreach(var subItem in UnitySubscriptions)
            {
                if(subItem.m_enabled)
                {
                    SubList.Subscriptions.Add(new Subscription(subItem.m_messageType, subItem.m_sender, subItem.m_id, subItem.m_subid, subItem.m_location, new MessageVersionSemantic(subItem.m_versionSemantic)));
                }
            }
            return SubList;
        }

        static byte[] EncryptStringToBytes_Aes(string plainText, byte[] Key, byte[] IV)
        {
            // Check arguments.
            if (plainText == null || plainText.Length <= 0)
                throw new ArgumentNullException("plainText");
            if (Key == null || Key.Length <= 0)
                throw new ArgumentNullException("Key");
            if (IV == null || IV.Length <= 0)
                throw new ArgumentNullException("IV");
            byte[] encrypted;

            // Create an Aes object
            // with the specified key and IV.
            using (Aes aesAlg = Aes.Create())
            {
                aesAlg.Key = Key;
                aesAlg.IV = IV;

                // Create an encryptor to perform the stream transform.
                ICryptoTransform encryptor = aesAlg.CreateEncryptor(aesAlg.Key, aesAlg.IV);

                // Create the streams used for encryption.
                using (MemoryStream msEncrypt = new MemoryStream())
                {
                    using (CryptoStream csEncrypt = new CryptoStream(msEncrypt, encryptor, CryptoStreamMode.Write))
                    {
                        using (StreamWriter swEncrypt = new StreamWriter(csEncrypt))
                        {
                            //Write all data to the stream.
                            swEncrypt.Write(plainText);
                        }
                        encrypted = msEncrypt.ToArray();
                    }
                }
            }

            // Return the encrypted bytes from the memory stream.
            return encrypted;
        }

        static string DecryptStringFromBytes_Aes(byte[] cipherText, byte[] Key, byte[] IV)
        {
            // Check arguments.
            if (cipherText == null || cipherText.Length <= 0)
                throw new ArgumentNullException("cipherText");
            if (Key == null || Key.Length <= 0)
                throw new ArgumentNullException("Key");
            if (IV == null || IV.Length <= 0)
                throw new ArgumentNullException("IV");

            // Declare the string used to hold
            // the decrypted text.
            string plaintext = null;

            // Create an Aes object
            // with the specified key and IV.
            using (Aes aesAlg = Aes.Create())
            {
                aesAlg.Key = Key;
                aesAlg.IV = IV;

                // Create a decryptor to perform the stream transform.
                ICryptoTransform decryptor = aesAlg.CreateDecryptor(aesAlg.Key, aesAlg.IV);

                // Create the streams used for decryption.
                using (MemoryStream msDecrypt = new MemoryStream(cipherText))
                {
                    using (CryptoStream csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read))
                    {
                        using (StreamReader srDecrypt = new StreamReader(csDecrypt))
                        {

                            // Read the decrypted bytes from the decrypting stream
                            // and place them in a string.
                            plaintext = srDecrypt.ReadToEnd();
                        }
                    }
                }
            }

            return plaintext;
        }
    }
}
