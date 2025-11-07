unit Unit22;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OpenGL;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    DC: HDC;
    RC: HGLRC;
    Angle: GLfloat;
    texBase, texSpider: GLuint;
    procedure SetDCPixelFormat(hdc: HDC);
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure Draw3D;
    function LoadTexture(const FileName: string): GLuint;
    procedure DrawCube(Size: GLfloat; TexID: GLuint);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  GL_CLAMP_TO_EDGE = $812F;

procedure TForm1.SetDCPixelFormat(hdc: HDC);
var
  pfd: TPixelFormatDescriptor;
  pf: Integer;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 24;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(hdc, @pfd);
  SetPixelFormat(hdc, pf, @pfd);
end;

function TForm1.LoadTexture(const FileName: string): GLuint;
var
  bmp: TBitmap;
  texID: GLuint;
  data: array of Byte;
  i, j, index: Integer;
  c, keyColor: TColor;
  r, g, b: Byte;
begin
  Result := 0;
  if not FileExists(FileName) then Exit;
  bmp := TBitmap.Create;
  try
    bmp.LoadFromFile(FileName);
    bmp.PixelFormat := pf24bit;
    SetLength(data, bmp.Width * bmp.Height * 4);
    keyColor := bmp.Canvas.Pixels[0, 0];
    index := 0;
    for j := bmp.Height - 1 downto 0 do
      for i := 0 to bmp.Width - 1 do
      begin
        c := bmp.Canvas.Pixels[i, j];
        r := GetRValue(c);
        g := GetGValue(c);
        b := GetBValue(c);
        data[index] := r; Inc(index);
        data[index] := g; Inc(index);
        data[index] := b; Inc(index);
        if c = keyColor then
          data[index] := 0
        else
          data[index] := 255;
        Inc(index);
      end;
    glGenTextures(1, @texID);
    glBindTexture(GL_TEXTURE_2D, texID);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, bmp.Width, bmp.Height, 0, GL_RGBA,
      GL_UNSIGNED_BYTE, @data[0]);
    Result := texID;
  finally
    bmp.Free;
  end;
end;

procedure TForm1.InitOpenGL;
begin
  DC := GetDC(Handle);
  SetDCPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);
  glClearColor(0.0, 0.0, 0.1, 1.0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glShadeModel(GL_SMOOTH);
  SetupProjection;
  texBase := LoadTexture('C:\Users\Darina\Documents\Embarcadero\Studio\Projects\Лаб 4\текстура1.bmp');
  texSpider := LoadTexture('C:\Users\Darina\Documents\Embarcadero\Studio\Projects\lab5\spider.bmp');
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

procedure TForm1.SetupProjection;
var
  aspect: GLfloat;
begin
  if ClientHeight = 0 then aspect := 1
  else aspect := ClientWidth / ClientHeight;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, aspect, 1.0, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TForm1.DrawCube(Size: GLfloat; TexID: GLuint);
var
  h: GLfloat;
begin
  h := Size;
  glBindTexture(GL_TEXTURE_2D, TexID);
  glBegin(GL_QUADS);
  glTexCoord2f(1, 1); glVertex3f( h,  h,  h);
  glTexCoord2f(0, 1); glVertex3f(-h,  h,  h);
  glTexCoord2f(0, 0); glVertex3f(-h, -h,  h);
  glTexCoord2f(1, 0); glVertex3f( h, -h,  h);
  glTexCoord2f(1, 1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0, 1); glVertex3f( h,  h, -h);
  glTexCoord2f(0, 0); glVertex3f( h, -h, -h);
  glTexCoord2f(1, 0); glVertex3f(-h, -h, -h);
  glTexCoord2f(1, 1); glVertex3f(-h,  h,  h);
  glTexCoord2f(0, 1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0, 0); glVertex3f(-h, -h, -h);
  glTexCoord2f(1, 0); glVertex3f(-h, -h,  h);
  glTexCoord2f(1, 1); glVertex3f( h,  h, -h);
  glTexCoord2f(0, 1); glVertex3f( h,  h,  h);
  glTexCoord2f(0, 0); glVertex3f( h, -h,  h);
  glTexCoord2f(1, 0); glVertex3f( h, -h, -h);
  glTexCoord2f(1, 1); glVertex3f( h,  h, -h);
  glTexCoord2f(0, 1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0, 0); glVertex3f(-h,  h,  h);
  glTexCoord2f(1, 0); glVertex3f( h,  h,  h);
  glTexCoord2f(1, 1); glVertex3f( h, -h,  h);
  glTexCoord2f(0, 1); glVertex3f(-h, -h,  h);
  glTexCoord2f(0, 0); glVertex3f(-h, -h, -h);
  glTexCoord2f(1, 0); glVertex3f( h, -h, -h);
  glEnd;
end;

procedure TForm1.Draw3D;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0, 0, -6);
  glRotatef(Angle, 1, 1, 0);
  if texBase <> 0 then
  begin
    glDepthMask(GL_TRUE);
    glDisable(GL_BLEND);
    DrawCube(1.0, texBase);
  end;
  if texSpider <> 0 then
  begin
    glEnable(GL_BLEND);
    glDepthMask(GL_FALSE);
    glPushMatrix;
    glTranslatef(0.0, 0.0, 0.01);
    DrawCube(1.0, texSpider);
    glPopMatrix;
    glDepthMask(GL_TRUE);
    glDisable(GL_BLEND);
  end;
  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0;
  Timer1.Interval := 16;
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if RC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(RC);
    RC := 0;
  end;
  if DC <> 0 then
    ReleaseDC(Handle, DC);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  glViewport(0, 0, ClientWidth, ClientHeight);
  SetupProjection;
  Draw3D;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Draw3D;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 1;
  if Angle >= 360 then Angle := 0;
  Draw3D;
end;

end.

