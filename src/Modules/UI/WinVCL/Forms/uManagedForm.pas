{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2021 Sensoft

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 ---------------------------------------------------------------------------------}

unit uManagedForm;

interface

uses
  Forms, uInteractor;

type
  TManagedForm = class;
  TManagedFormClass = class of TManagedForm;

  TManagedForm = class(TForm)
  protected
    FInteractor: TInteractor;
    procedure DoInit(const AInteractor: TInteractor); virtual;
  public
    class procedure ShowPage(const APageClass: TManagedFormClass; const AInteractor: TInteractor);
  end;

implementation

uses
  SysUtils;

{ TManagedForm }

procedure TManagedForm.DoInit(const AInteractor: TInteractor);
begin
  FInteractor := AInteractor;
end;

class procedure TManagedForm.ShowPage(const APageClass: TManagedFormClass; const AInteractor: TInteractor);
var
  vForm: TManagedForm;
begin
  Application.CreateForm(APageClass, vForm);
  try
    vForm.DoInit(AInteractor);
    vForm.ShowModal;
  finally
    FreeAndNil(vForm);
  end;
end;

end.

