using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using UnityEngine;
using UnityEngine.UI;
using System.Linq;

class Message
{


    public class Item
    {
        public int Id;
        public string Name;
    }
    public static void testtt()
    {
        // int 4个字节(byte) 
        // long 8个字节(byte) 
        int a = 98598998; //a=98598998
        
        // 把int转成byte数组
        byte[] arr = BitConverter.GetBytes(a);
        for (int i = 0; i < arr.Length; i++)
        {
            Debug.Log(string.Format("arr{0} = {1}",i,arr[i]));
        }

    }
    public static void testtt2()
    {
        byte[] arr = new byte[4];
        arr[0] = 86;
        arr[1] = 128;
        arr[2] = 224;
        arr[3] = 5;
        int a = BitConverter.ToInt32(arr,0);
        Debug.Log("a=" + a); // a=98598998
    }
    public static void testtt3()
    {
        Item item = new Item() { Id = 1, Name = "啊我" };
        byte[] arr = null;
        using (MMO_MemoryStream ms = new MMO_MemoryStream())
        {
            ms.WriteInt(item.Id);
            ms.WriteUTF8String(item.Name);
            arr = ms.ToArray();
        }

        // if (arr != null)
        // {
        //     for (int i = 0; i < arr.Length; i++)
        //     {
        //         Debug.Log(string.Format("arr{0} = {1}",i,arr[i]));
        //     }
        //
        // }
        
        Item item2 = new Item() ;
       
        using (MMO_MemoryStream ms = new MMO_MemoryStream(arr))
        {
            item2.Id = ms.ReadInt();
            item2.Name = ms.ReadUTF8String();
        }
        Debug.Log(string.Format("arr{0}", item2.Id));
        Debug.Log(string.Format("arr{0}", item2.Name));
        
       
    }
    // 1代表小端 0代表大端
    public static int IsBigByte()
    {
        int i = 1;
 
        //将int 转换到数组中，然后根据数组的地址进行判断
        byte[] buf = BitConverter.GetBytes(i);
 
        if(buf[0] == 1)
        {    
            return 1;
        }else{
            return 0;
        }
    }
    public static byte[] GetBytes(ushort cmd, string data)
    {
        // 小端需要逆序
        int isBigByte ;
        isBigByte = IsBigByte();
        
        // 协议号
        byte[] tempRequestCodeBytes = BitConverter.GetBytes(cmd);
        byte[] requestCodeBytes = (isBigByte == 1 ? tempRequestCodeBytes.Reverse().ToArray() : tempRequestCodeBytes);
        // 数据
        byte[] dataBytes = Encoding.UTF8.GetBytes(data); //此方法不需要转
        // byte[] dataBytes = (isBigByte == 1 ? tempDataBytes.Reverse().ToArray() : tempDataBytes);
        // 数据长度+4
        ushort dataAmount = (ushort)(dataBytes.Length + 4);
        byte[] tempDataAmountBytes = BitConverter.GetBytes(dataAmount);
        byte[] dataAmountBytes = (isBigByte == 1 ? tempDataAmountBytes.Reverse().ToArray() : tempDataAmountBytes);
        
        return  dataAmountBytes.Concat(requestCodeBytes).Concat(dataBytes).ToArray();
        
        // 先取4个字节拿到数据长度+4和协议号，然后再解码数据
        // <<Len:16, Cmd:16, Data/binary>>.
        // [数据长度+4(2字节) + 协议(2字节) + 数据]
        // [数据 + 协议 + 数据总长度]
        // String output = Convert.ToInt32(input, 2).ToString();
    }
    
  
}

public class ChatManager : MonoBehaviour
{
    private string ipaddress = "127.0.0.1";
    private int port = 7788;
    public InputField textInput;
    public Text showText;
    private Socket clientSocket;
    private Thread t;
    private string message = "";
    private byte[] data = new byte[1024];
    // Start is called before the first frame update
    void Start()
    {
      //  ConnectToServer();
      Message.testtt3();
    }

    // Update is called once per frame
    void Update()
    {

        if (message != null && message != "")
        {
            showText.text += "\n" + message;
            message = "";
        }
    }

    void ConnectToServer()
    {
        clientSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        clientSocket.Connect(new IPEndPoint(IPAddress.Parse(ipaddress), port));
        
        t = new Thread(ReceiveMessage);
        t.Start();
        
        
        
    }
    private void ReceiveMessage()
    {
        while (true)
        {

            if (clientSocket.Poll(10, SelectMode.SelectRead))
            {
                clientSocket.Close();
                break;
            }
            // if (clientSocket.Connected == false)
            // {
            //     break;
            // }
            
            int length = clientSocket.Receive(data);
            message = Encoding.UTF8.GetString(data, 0, length);
           

        }
    }

    void SendCvMessage(ushort cmd,string message)
    {
        clientSocket.Send(Message.GetBytes(cmd,message));
    }

    public void OnSendButtonClick()
    {
        string value = textInput.text;
        SendCvMessage(1,value);
        textInput.text = "";
    }

    private void OnDestroy()
    {
        
        clientSocket.Shutdown(SocketShutdown.Both);
        clientSocket.Close();
    }
    
    // ==========================测试按钮===========================================
    public void OnLoginButtonClick()
    {
        SendCvMessage(10000,"A1547");
    }

}