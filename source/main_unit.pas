{
 Antenna Rotator Control Software
 Version 1.0, September 2023
 By Bertrand Zauhar, VE2ZAZ / VA2IW
  https://ve2zaz.net
  https://github.com/VE2ZAZ
  https://www.qrz.com/db/VE2ZAZ

This software is distributed under the “Creative Commons Attribution 4.0 International (CC BY 4.0)” license agreement.
For more detail, please consult the following page: https://creativecommons.org/licenses/by/4.0/
}

unit Main_Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, LazSerial, StdCtrls, Menus, Types, Math, blcksock, XMLConf,
  LCLIntf, LCLType, ExtDlgs,synaser;

type

  { TMain_Form }

  TMain_Form = class(TForm)
    Change_Image_Button: TButton;
    Globe_Image: TImage;
    Config_Panel: TPanel;
    OpenPictureDialog: TOpenPictureDialog;
    Set_Serial_Button: TButton;
    Mem_2_Button: TButton;
    Mem_3_Button: TButton;
    Mem_4_Button: TButton;
    Mem_5_Button: TButton;
    Mem_6_Button: TButton;
    Mem_7_Button: TButton;
    Mem_8_Button: TButton;
    Mem_Edit_Edit_Label: TLabel;
    Mem_Edit_Panel: TPanel;
    Mem_Edit_Edit: TEdit;
    Mem_1_Button: TButton;
    Heading_Edit: TEdit;
    Target_Edit_Label: TLabel;
    LazSerial: TLazSerial;
    Az_Needle_PaintBox: TPaintBox;
    Controls_Panel: TPanel;
    Mem_Buttons_Panel: TPanel;
    Help_Button: TButton;
    Stop_BitBtn: TBitBtn;
    Rotate_BitBtn: TBitBtn;
    Az_Read_Timer: TTimer;
    Refresh_Timer: TTimer;
    Socket_Timer: TTimer;
    XMLConfig: TXMLConfig;
    procedure Az_Needle_PaintBox_OnMouse_Enter(Sender: TObject);
    procedure Az_Needle_PaintBox_OnMouse_Leave(Sender: TObject);
    procedure Az_PaintBox_OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Change_Image_Button_OnClick(Sender: TObject);
    procedure Help_Button_OnClick(Sender: TObject);
    procedure Main_Form_OnActivate(Sender: TObject);
    procedure Mem_Edit_Panel_OnPaint(Sender: TObject);
    procedure LazSerial_Set_Button_OnClick(Sender: TObject);
    procedure Main_Form_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Heading_EditChange(Sender: TObject);
    procedure Heading_Edit_OnKey_Press(Sender: TObject; var Key: char);
    procedure LazSerialRxData(Sender: TObject);
    procedure Mem_1_ButtonClick(Sender: TObject);
    procedure Mem_2_ButtonClick(Sender: TObject);
    procedure Mem_3_ButtonClick(Sender: TObject);
    procedure Mem_4_ButtonClick(Sender: TObject);
    procedure Mem_5_ButtonClick(Sender: TObject);
    procedure Mem_6_ButtonClick(Sender: TObject);
    procedure Mem_7_ButtonClick(Sender: TObject);
    procedure Mem_8_ButtonClick(Sender: TObject);
    procedure Mem_Edit_Edit_OnChange(Sender: TObject);
    procedure Mem_Edit_Edit_OnKeyPress(Sender: TObject; var Key: char);
    procedure Mem_X_Button_OnMouse_Down(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Read_Timer_OnTimer(Sender: TObject);
    procedure Refresh_Timer_OnTimer(Sender: TObject);
    procedure Rotate_BitBtnClick(Sender: TObject);
    procedure Rotate_to_Heading;
    procedure Socket_Timer_OnTimer(Sender: TObject);
    procedure Stop_Rotation;
    procedure Stop_BitBtn_OnClick(Sender: TObject);
    procedure Main_Form_OnPaint(Sender: TObject);
    procedure Repaint_Az_Needle;
    procedure RestoreFormState;
    procedure StoreFormState;
    procedure Open_serial_Port;

  private

  public
    function Get_Linux_SerialPortNames: string;
  end;

var
  Main_Form: TMain_Form;
  az_string: String;
  az_value: integer;
  az_angle: integer;
  old_az_value: integer = 361;
  old_cursor_x: integer = 10000;
  old_cursor_y: integer = 10000;
  radius: Integer;   // Radius of the circle
  center: TPoint;    // Center point of the circle
  R: TRect;          // Rectangle enclosing the circle
  mouse_on_paintbox: Boolean;
  cursor_x, cursor_y: Integer;
  sock: TUDPBlockSocket;
  Mem_Button_Sender: TObject;
  settings_file: TextFile;
  serial_port: String;
  serial_fail: Boolean;
  Image_Filename: String;

implementation

{$R *.lfm}

{ TMain_Form }
procedure TMain_Form.Repaint_Az_Needle;
var
  x_needle,y_needle, ww, hh: integer;
  x0,x1,x2,y0,y1,y2: integer;
  A,B,C: Real;
  cursor_angle: integer;
begin
  // Adjust globe image size
  Globe_Image.Width := 2 * radius;
  Globe_Image.Height := 2 * radius;
  Globe_Image.Left := 5;
  Globe_Image.Top := 5;
  Globe_Image.SendToBack;

  // Adjust the needle paintbox size accordingly
  Az_Needle_PaintBox.Left := center.X - radius;
  Az_Needle_PaintBox.Top := center.Y - radius;
  Az_Needle_PaintBox.Width := 2 * radius;
  Az_Needle_PaintBox.Height := 2 * radius;
  Az_Needle_PaintBox.RePaint;                      // Erases tha canvas

    // Paint the circle
  R := Rect(center.X - radius, center.Y - radius, center.X + radius, center.Y + radius);
  If not Globe_Image.Visible Then
  begin
       Main_Form.Canvas.Brush.Color := clSkyBlue; //clBlue;
       Main_Form.Canvas.Brush.Style := bsSolid;
       Main_Form.Canvas.Ellipse(R);              // Wipes out everythig by redrawing the circle
  end;
  // Paint the azimuth needle and heading text
  Az_Needle_PaintBox.Canvas.pen.Width := 3;
  Az_Needle_PaintBox.Canvas.pen.Color:= clNavy;
  x_needle := Round(cos(az_value * pi / 180 - pi / 2) * (radius) + radius);
  y_needle := Round(sin(az_value * pi / 180 - pi / 2) * (radius) + radius);
  if (az_value > 180) then x_needle := x_needle + 5 else x_needle := x_needle - 5;                       // Trim line length to avoid spillover due to rounding up.
  if ((az_value > 90) and (az_value < 270)) then y_needle := y_needle - 5 else y_needle := y_needle + 5;
  if (not serial_fail) then Az_Needle_PaintBox.Canvas.Line(radius, radius, x_needle, y_needle);
  Az_Needle_PaintBox.Canvas.Font.Color:=clNavy;   //;
  Az_Needle_PaintBox.Canvas.Font.Size:= Round(Az_Needle_PaintBox.Width / 18);
  if Az_Needle_PaintBox.Canvas.Font.Size < 16 then Az_Needle_PaintBox.Canvas.Font.Size := 14;
  Az_Needle_PaintBox.Canvas.Font.Style := [fsBold];
  Az_Needle_PaintBox.Canvas.Brush.Style:=bsClear;
  ww := Az_Needle_PaintBox.Canvas.TextWidth(az_string);
  hh := Az_Needle_PaintBox.Canvas.TextHeight(az_string);
  if ((not mouse_on_paintbox) and (not serial_fail)) then Az_Needle_PaintBox.Canvas.TextOut(radius - round(ww/2), radius - Round(hh/2), az_string);

  // Now work on painting the target azimuth needle and text
  if (mouse_on_paintbox) then
  begin
    // Calculate the angle (azimuth) created by the mouse cursor
    x0 := radius;
    y0 := radius;
    x1 := radius;
    y1 := 0;
    x2 := cursor_x;
    y2 := cursor_y;
    A := Sqrt(Sqr(x2-x1) + Sqr(y2-y1));
    B := Sqrt(Sqr(x2-x0) + Sqr(y2-y0));
    C := Sqrt(Sqr(x1-x0) + Sqr(y1-y0));
    cursor_angle := Round(180 - ArcCos((Sqr(A)-Sqr(B)-Sqr(C))/(2*B*C)) * 180 / (pi));
    if (x2 <= radius) then cursor_angle := 359 - cursor_angle;
    if cursor_angle > 180 then    // Removes the jumpiness of the needle around 0 degrees
    begin
      x_needle := Trunc(cos(cursor_angle * pi / 180 - pi / 2) * radius + radius);
      y_needle := Trunc(sin(cursor_angle * pi / 180 - pi / 2) * radius + radius);
    end
    else begin
      x_needle := Round(cos(cursor_angle * pi / 180 - pi / 2) * radius + radius);
      y_needle := Round(sin(cursor_angle * pi / 180 - pi / 2) * radius + radius);
    end;
    if (cursor_angle > 180) then x_needle := x_needle + 5 else x_needle := x_needle - 5;     // Trim line length to avoid spillover due to rounding up.
    if ((cursor_angle > 90) and (cursor_angle < 270)) then y_needle := y_needle - 5 else y_needle := y_needle + 5;
    Az_Needle_PaintBox.Canvas.Pen.Width := 3;
    Az_Needle_PaintBox.Canvas.Pen.Color:= clRed;
    Az_Needle_PaintBox.Canvas.Pen.Style := psDot;
    Az_Needle_PaintBox.Canvas.Line(radius, radius, x_needle, y_needle);

    Heading_Edit.Text := IntToStr(cursor_angle);
    Az_Needle_PaintBox.Canvas.Font.Color:=clRed;
    ww := Az_Needle_PaintBox.Canvas.TextWidth(Heading_Edit.Text);
    hh := Az_Needle_PaintBox.Canvas.TextHeight(Heading_Edit.Text);
    //    Az_Needle_PaintBox.Canvas.TextOut(radius - round(ww/2), radius - Round(hh/2), Heading_Edit.Text);
    if (cursor_angle <= 90) then Az_Needle_PaintBox.Canvas.TextOut(Round(radius + (0.75 * radius * sin(cursor_angle/180*pi)) - (ww/2)), Round((radius) + (0.75 * radius * -cos(cursor_angle/180*pi)) - (hh/2)), Heading_Edit.Text)
    else if (cursor_angle > 90) and (cursor_angle <= 180) then Az_Needle_PaintBox.Canvas.TextOut(Round(radius + (0.75 * radius * sin(cursor_angle/180*pi)) - (ww/2)), Round(radius + (0.75 * radius * -cos(cursor_angle/180*pi)) - (hh/2)), Heading_Edit.Text)
    else if (cursor_angle > 180) and (cursor_angle <= 270) then Az_Needle_PaintBox.Canvas.TextOut(Round(radius + (0.75 * radius * sin(cursor_angle/180*pi)) - (ww/2)), Round(radius + (0.75 * radius * -cos(cursor_angle/180*pi)) - (hh/2)), Heading_Edit.Text)
    else Az_Needle_PaintBox.Canvas.TextOut(Round(radius + (0.75 * radius * sin(cursor_angle/180*pi)) - (ww/2)), Round(radius + (0.75 * radius * -cos(cursor_angle/180*pi)) - (hh/2)), Heading_Edit.Text)
  end;
end;

procedure TMain_Form.LazSerialRxData(Sender: TObject);
begin
      az_string := LazSerial.ReadData();
      az_string := Copy(az_string,2);             // Remove the ';' before the heading
      az_string := Copy(az_string,0,3);             // Remove anything after heading
      Val(az_string,az_value);
end;

procedure TMain_Form.Mem_1_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_1_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_2_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_2_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_3_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_3_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_4_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_4_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_5_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_5_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_6_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_6_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_7_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_7_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_8_ButtonClick(Sender: TObject);
begin
  Heading_Edit.Text := Mem_8_Button.Caption;
  Rotate_to_Heading;
end;

procedure TMain_Form.Mem_Edit_Edit_OnChange(Sender: TObject);
var
temp_val: integer;
code: integer;
temp_text: String;
begin
  Val(Mem_Edit_Edit.Text,temp_val,code);
  if ((temp_val < 0) or (temp_val > 359)) then
  begin
    temp_text := Mem_Edit_Edit.Text;
    Delete(temp_text, Length(temp_text) - 1, 1);
    Mem_Edit_Edit.Text := temp_text;
  end;
end;

procedure TMain_Form.Mem_Edit_Edit_OnKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 13 then
  begin
    (Mem_Button_Sender as TButton).Caption := Mem_Edit_Edit.Text;
    Mem_Buttons_Panel.Visible := True;
    Mem_Edit_Panel.Visible := False;
    StoreFormState;
  end
  else if Ord(Key) = 27 then           // Escape key
  begin
    Mem_Buttons_Panel.Visible := True;
    Mem_Edit_Panel.Visible := False;
  end;
end;

procedure TMain_Form.Mem_X_Button_OnMouse_Down(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Button = mbRight then
  begin
      Mem_Button_Sender := Sender;
      Mem_Buttons_Panel.Visible := False;
      Mem_Edit_Panel.Left := Mem_Buttons_Panel.Left;
      Mem_Edit_Panel.Top := Mem_Buttons_Panel.Top;
      Mem_Edit_Panel.Visible := True;
      Mem_Edit_Edit.Text := (Sender as TButton).Caption;
      // Mem_Edit_Edit.SetFocus;    // Does not work here. Put in Panel OnPaint event fuction.
  end;
end;

procedure TMain_Form.Heading_EditChange(Sender: TObject);
var
temp_val: integer;
code: integer;
temp_text: String;

begin
  Val(Heading_Edit.Text,temp_val,code);
  if ((temp_val < 0) or (temp_val > 359)) then
  begin
    temp_text := Heading_Edit.Text;
    Delete(temp_text, Length(temp_text) - 1, 1);
    Heading_Edit.Text := temp_text;
  end;
end;

procedure TMain_Form.Az_Needle_PaintBox_OnMouse_Enter(Sender: TObject);
begin
  mouse_on_paintbox := True;
end;

procedure TMain_Form.Az_Needle_PaintBox_OnMouse_Leave(Sender: TObject);
begin
  mouse_on_paintbox := False;
  Repaint_Az_Needle;         // Required to cleanup the target needle, which sometimes does not clear on mouse leave
end;

procedure TMain_Form.Az_PaintBox_OnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
     cursor_x := X;
     cursor_y := Y;
end;

procedure TMain_Form.Change_Image_Button_OnClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    if fileExists(OpenPictureDialog.Filename) then
    begin
      Globe_Image.Picture.LoadFromFile(OpenPictureDialog.Filename);
      Image_Filename := ExtractFileName(OpenPictureDialog.Filename);
      Globe_Image.Visible := True;
    end
  end
  else begin
  Globe_Image.Visible := False;
  Image_Filename := '';
  end;
end;


procedure TMain_Form.Help_Button_OnClick(Sender: TObject);
begin
  OpenURL('./help/help.pdf');
end;

procedure TMain_Form.Main_Form_OnActivate(Sender: TObject);
begin
  if Main_Form.Width > Main_Form.Height then radius := Round(Main_Form.Height * 0.3)
  else radius := Round(Main_Form.Width * 0.3);

  // Connect to UDP broadcast address with socket to receive N1MM heading messages
  sock := TUDPBlockSocket.Create;
  sock.createsocket;
  sock.Bind('0.0.0.0', '12040');
  if sock.LastError <> 0 then             // Was there an error?
  begin
    ShowMessage('N1MM server unreachable.');
  end;
  RestoreFormState;                     // Restore main window location and size, and program configuration
  Az_Read_Timer.Enabled := True;
  Socket_Timer.Enabled := True;
  Refresh_Timer.Enabled := True;
  try
    Globe_Image.Picture.LoadFromFile(Image_Filename);
    Globe_Image.Visible := True;
  except
    Globe_Image.Visible := False;
  end;
  Open_serial_Port;
end;

procedure TMain_Form.Mem_Edit_Panel_OnPaint(Sender: TObject);  // A workaround function to set focus to the edit box on Panel paint. Will not work anywhere else
begin
  Mem_Edit_Edit.SetFocus;
end;

procedure TMain_Form.LazSerial_Set_Button_OnClick(Sender: TObject);
begin
  LazSerial.ShowSetupDialog;
  Open_Serial_Port;
  StoreFormState;
end;

procedure TMain_Form.Main_Form_OnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Socket_Timer.Enabled := False;
  sock.Free;
  LazSerial.Close;
  StoreFormState;
end;

procedure TMain_Form.Heading_Edit_OnKey_Press(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 13 then Rotate_to_Heading;
end;

procedure TMain_Form.Read_Timer_OnTimer(Sender: TObject);
begin
  LazSerial.WriteData('AI1;');        // Send azimuth request to Rotator controller
end;

procedure TMain_Form.Refresh_Timer_OnTimer(Sender: TObject);
begin
  if (az_value <> old_az_value) or (mouse_on_paintbox and (old_cursor_x <> cursor_x) or (old_cursor_y <> cursor_y)) then
  begin
    Repaint_Az_Needle;
    old_az_value := az_value;
    old_cursor_x := cursor_x;
    old_cursor_y := cursor_y;
  end;
end;

procedure TMain_Form.Rotate_BitBtnClick(Sender: TObject);
begin
    Rotate_to_Heading;
end;

procedure TMain_Form.Rotate_to_Heading;
begin
  az_string:=  Heading_Edit.Text;
  LazSerial.WriteData('AP1' + az_string + chr(13));
end;

procedure TMain_Form.Socket_Timer_OnTimer(Sender: TObject);
var
   rx_string, rx_substring: String;
   azimuth_string: String;
   azimuth_string_index_begin,
   azimuth_string_index_end: Integer;

begin
  rx_string := '';
  repeat
    rx_substring := sock.RecvPacket(10);
    rx_string := rx_string + rx_substring;
  until rx_substring = '';

  if (Pos('<stop>',rx_string) <> 0) then Stop_Rotation
  else if Pos('<goazi>',rx_string) <> 0 then
  begin
    azimuth_string_index_begin := Pos('<goazi>',rx_string) + Length('<goazi>');
    azimuth_string_index_end := Pos('</goazi>',rx_string);
    azimuth_string := Copy(rx_string, azimuth_string_index_begin, azimuth_string_index_end - azimuth_string_index_begin);
    azimuth_string := azimuth_string.Split('.')[0];
    az_string:= azimuth_string;
    Heading_Edit.Text := azimuth_string;
    Rotate_to_Heading;
  end;
end;

procedure TMain_Form.Stop_Rotation;
begin
  LazSerial.WriteData(';');
end;

procedure TMain_Form.Stop_BitBtn_OnClick(Sender: TObject);
begin
  Stop_Rotation;
end;

procedure TMain_Form.Main_Form_OnPaint(Sender: TObject);
begin
  if (Main_Form.Height < Controls_Panel.Height + 20) then Main_Form.Height := Controls_Panel.Height + 20;
  // The radius of the circle, is 1/2 of form width, minus some gap to the border
  if Main_Form.Width > Main_Form.Height then
    radius := (Main_Form.Height div 2) - GetSystemMetrics(SM_CYHSCROLL) // scrollbar height
  else
    radius := (Main_Form.Width div 2) - 5;

  // Position the circle at the top-left corner of the form, with a gap to the border added
  center.X := radius + 5;
  center.Y := radius + 5;

  // Position the button panel vs. the circle
  Controls_Panel.Left := (radius * 2) + 15;
  Controls_Panel.Top := (((radius * 2) - Controls_Panel.Height) div 2) + 5;
  if (Controls_Panel.Top < 5) then Controls_Panel.Top := 5;

  Mem_Buttons_Panel.Left := Controls_Panel.Left + Controls_Panel.Width + 5;
  Mem_Buttons_Panel.Top :=  Controls_Panel.Top;

  Mem_Edit_Panel.Left := Mem_Buttons_Panel.Left;
  Mem_Edit_Panel.Top := Mem_Buttons_Panel.Top;

  Config_Panel.Left := Mem_Buttons_Panel.Left + Mem_Buttons_Panel.Width + 5;
  Config_Panel.Top :=  Mem_Buttons_Panel.Top;
end;

procedure TMain_Form.StoreFormState;
begin
  with XMLConfig do begin
    SetValue('NormalLeft', Left);
    SetValue('NormalTop', Top);
    SetValue('NormalWidth', Width);
    SetValue('NormalHeight', Height);
    SetValue('RestoredLeft', RestoredLeft);
    SetValue('RestoredTop', RestoredTop);
    SetValue('RestoredWidth', RestoredWidth);
    SetValue('RestoredHeight', RestoredHeight);
    SetValue('WindowState', Integer(WindowState));

    // Added for program specific settings
    SetValue('SerialPort', LazSerial.Device);
    SetValue('ImageFilename', Image_Filename);
    SetValue('Mem1Value', Mem_1_Button.Caption);
    SetValue('Mem2Value', Mem_2_Button.Caption);
    SetValue('Mem3Value', Mem_3_Button.Caption);
    SetValue('Mem4Value', Mem_4_Button.Caption);
    SetValue('Mem5Value', Mem_5_Button.Caption);
    SetValue('Mem6Value', Mem_6_Button.Caption);
    SetValue('Mem7Value', Mem_7_Button.Caption);
    SetValue('Mem8Value', Mem_8_Button.Caption);
  end;
end;

procedure TMain_Form.RestoreFormState;
var
  LastWindowState: TWindowState;
begin
  with XMLConfig do begin
    LastWindowState := TWindowState(GetValue('WindowState', Integer(WindowState)));
    if LastWindowState = wsMaximized then begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('RestoredLeft', RestoredLeft),
        GetValue('RestoredTop', RestoredTop),
        GetValue('RestoredWidth', RestoredWidth),
        GetValue('RestoredHeight', RestoredHeight));
      WindowState := wsMaximized;
    end else begin
      WindowState := wsNormal;
      BoundsRect := Bounds(
        GetValue('NormalLeft', Left),
        GetValue('NormalTop', Top),
        GetValue('NormalWidth', Width),
        GetValue('NormalHeight', Height));
    end;
  end;
  // Added for program specific settings
  Mem_1_Button.Caption := XmlConfig.GetValue('Mem1Value','0');
  Mem_2_Button.Caption := XmlConfig.GetValue('Mem2Value','0');
  Mem_3_Button.Caption := XmlConfig.GetValue('Mem3Value','0');
  Mem_4_Button.Caption := XmlConfig.GetValue('Mem4Value','0');
  Mem_5_Button.Caption := XmlConfig.GetValue('Mem5Value','0');
  Mem_6_Button.Caption := XmlConfig.GetValue('Mem6Value','0');
  Mem_7_Button.Caption := XmlConfig.GetValue('Mem7Value','0');
  Mem_8_Button.Caption := XmlConfig.GetValue('Mem8Value','0');
  LazSerial.Device := XmlConfig.GetValue('SerialPort','/dev/ttyACM0');
  Image_Filename :=  XmlConfig.GetValue('ImageFilename','AzimuthalMap.png');
end;

procedure TMain_Form.Open_Serial_Port;
var
  Com_List: TStringList;
  Index: Integer;
begin
  Com_List := TStringList.Create;
  Com_List.delimiter := char(',');
{$ifdef Windows}
Com_List.text := synaser.GetSerialPortNames();
{$else}
Com_List.text := Get_Linux_SerialPortNames;
{$endif}
  Com_List.delimitedText := Com_List.text;
  Com_List.Sorted := true;
  if  Com_List.Find(LazSerial.Device,Index) = True then
  begin
    try
      LazSerial.Open;
      serial_fail := false;
      Controls_Panel.Enabled := True;
      Mem_Buttons_Panel.Enabled := True;
    except
      ShowMessage ('Serial Port ' + LazSerial.Device + ' cannot be opened.'+ sLineBreak + 'Please select a valid port.');
      serial_fail := True;
      Controls_Panel.Enabled := False;
      Mem_Buttons_Panel.Enabled := False;
    end;
  end
  else
  begin
    ShowMessage ('Serial Port ' + LazSerial.Device + ' is not valid.'+ sLineBreak + 'Please select a valid port.');
    serial_fail := True;
    Controls_Panel.Enabled := False;
    Mem_Buttons_Panel.Enabled := False;
  end;
  Com_List.Free;
end;

function TMain_Form.Get_Linux_SerialPortNames: string;
var
  sr : TSearchRec;
begin
  Result := '';
  if FindFirst('/dev/tty*', faAnyFile, sr) = 0 then
    repeat
      if (sr.Attr and $FFFFFFFF) = Sr.Attr then
      begin
         if (pos('S', sr.Name) > 0) or (pos('ACM', sr.Name) > 0) or (pos('USB', sr.Name) > 0) then
         begin
           Result := Result + '/dev/' + sr.Name + ',';
         end;
      end;
    until FindNext(sr) <> 0;
  FindClose(sr);
end;

end.
