unit Unit17;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  OpenGL, jpeg, Math;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    DC: HDC;
    RC: HGLRC;
    Angle: GLfloat;
    procedure InitGL;
    procedure Draw3D;
    procedure LoadJpegTextures;
  public
  end;

var
  Form1: TForm1;
  arrayRGB: array[0..511, 0..2047, 0..2] of GLubyte;
  Bmp: TBitmap;

const
  tw0 = 512;
  th0 = 512;
  n = 4;
  tw = n * tw0;
  th = th0;
  ImagePath = 'C:\Users\Darina\Documents\Embarcadero\Studio\Projects\lab5\';

implementation

{$R *.dfm}

procedure TForm1.InitGL;
var
  pfd: TPixelFormatDescriptor;
  pf: Integer;
begin
  DC := GetDC(Handle);
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 24;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(DC, @pfd);
  SetPixelFormat(DC, pf, @pfd);

  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glClearColor(0.0, 0.0, 0.15, 1.0);

  LoadJpegTextures;
end;

procedure TForm1.LoadJpegTextures;
var
  i, j, k: Integer;
  JPG: TJPEGImage;
  fname: string;
begin
  for k := 1 to n do
  begin
    fname := ImagePath + IntToStr(k) + '.jpg';
    if not FileExists(fname) then
      raise Exception.Create('Файл не знайдено: ' + fname);

    JPG := TJPEGImage.Create;
    try
      JPG.LoadFromFile(fname);
      Bmp := TBitmap.Create;
      Bmp.Width := tw0;
      Bmp.Height := th0;
      Bmp.Canvas.StretchDraw(Bmp.Canvas.ClipRect, JPG);
    finally
      JPG.Free;
    end;

    for i := 0 to tw0 - 1 do
      for j := 0 to th0 - 1 do
      begin
        arrayRGB[j, i + (k - 1) * tw0, 0] := GetRValue(Bmp.Canvas.Pixels[i, th0 - 1 - j]);
        arrayRGB[j, i + (k - 1) * tw0, 1] := GetGValue(Bmp.Canvas.Pixels[i, th0 - 1 - j]);
        arrayRGB[j, i + (k - 1) * tw0, 2] := GetBValue(Bmp.Canvas.Pixels[i, th0 - 1 - j]);
      end;
    Bmp.Free;
  end;

  gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, tw, th, GL_RGB, GL_UNSIGNED_BYTE, @arrayRGB);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
end;

procedure TForm1.Draw3D;
const
  h = 1.0;
var
  s: GLfloat;
begin
  s := Angle / 360;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0, 0, -4);
  glRotatef(Angle, 0, 1, 0);

  glBegin(GL_QUAD_STRIP);
  glTexCoord2d(0.0 + s, 1); glVertex3f(-h, h, h);
  glTexCoord2d(0.0 + s, 0); glVertex3f(-h, -h, h);
  glTexCoord2d(0.25 + s, 1); glVertex3f(h, h, h);
  glTexCoord2d(0.25 + s, 0); glVertex3f(h, -h, h);
  glTexCoord2d(0.5 + s, 1); glVertex3f(h, h, -h);
  glTexCoord2d(0.5 + s, 0); glVertex3f(h, -h, -h);
  glTexCoord2d(0.75 + s, 1); glVertex3f(-h, h, -h);
  glTexCoord2d(0.75 + s, 0); glVertex3f(-h, -h, -h);
  glTexCoord2d(1.0 + s, 1); glVertex3f(-h, h, h);
  glTexCoord2d(1.0 + s, 0); glVertex3f(-h, -h, h);
  glEnd;

  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitGL;
  Timer1.Interval := 16;
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if RC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(RC);
  end;
  if DC <> 0 then
    ReleaseDC(Handle, DC);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  glViewport(0, 0, ClientWidth, ClientHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45, ClientWidth / ClientHeight, 1, 100);
  glMatrixMode(GL_MODELVIEW);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 1;
  Repaint;
end;

end.

