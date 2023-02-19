object Form: TForm
  Tag = 1
  Left = 0
  Top = 0
  Width = 1067
  Height = 749
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlBase: TPanel
    AlignWithMargins = True
    Left = 16
    Top = 16
    Width = 1035
    Height = 717
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    Align = alClient
    TabOrder = 0
    object pnlTop: TPanel
      AlignWithMargins = True
      Left = 17
      Top = 17
      Width = 1001
      Height = 144
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      Align = alTop
      TabOrder = 0
      object Name: TPanel
        Left = 21
        Top = 28
        Width = 250
        Height = 24
        Caption = 'Name'
        TabOrder = 0
      end
      object pnlStreets: TPanel
        Left = 295
        Top = 28
        Width = 250
        Height = 97
        Caption = 'Streets'
        TabOrder = 1
      end
      object Panel5: TPanel
        Left = 21
        Top = 76
        Width = 100
        Height = 37
        Caption = 'ShowAbout'
        TabOrder = 2
      end
      object Panel6: TPanel
        Left = 160
        Top = 76
        Width = 111
        Height = 24
        Caption = 'ShowAbout?ViewStyle=link'
        TabOrder = 3
      end
      object Panel15: TPanel
        AlignWithMargins = True
        Left = 840
        Top = 15
        Width = 144
        Height = 114
        Margins.Top = 14
        Margins.Right = 16
        Margins.Bottom = 14
        Align = alRight
        Alignment = taRightJustify
        Caption = '$/State?view=info'
        ShowCaption = False
        TabOrder = 4
      end
      object Panel8: TPanel
        Left = 567
        Top = 28
        Width = 250
        Height = 97
        Caption = 'Owner?view=radio'
        TabOrder = 5
      end
    end
    object pgc1: TPageControl
      AlignWithMargins = True
      Left = 17
      Top = 193
      Width = 1001
      Height = 507
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      ActivePage = tsOthers
      Align = alClient
      TabOrder = 1
      object tsString: TTabSheet
        Caption = 'String'
        object Phone: TPanel
          Left = 20
          Top = 39
          Width = 250
          Height = 24
          Caption = 'Phone'
          TabOrder = 0
        end
        object Memo: TPanel
          Left = 580
          Top = 192
          Width = 250
          Height = 75
          Caption = 'Memo'
          TabOrder = 1
        end
        object Mru: TPanel
          Left = 287
          Top = 90
          Width = 250
          Height = 24
          Caption = 'Mru'
          TabOrder = 2
        end
        object Log: TPanel
          Left = 580
          Top = 39
          Width = 250
          Height = 100
          Caption = 'Log'
          TabOrder = 3
        end
        object INN: TPanel
          Left = 20
          Top = 192
          Width = 250
          Height = 24
          Caption = 'INN'
          TabOrder = 4
        end
        object Info: TPanel
          Left = 287
          Top = 39
          Width = 250
          Height = 24
          Caption = 'Info'
          TabOrder = 5
        end
        object ImageByString: TPanel
          Left = 287
          Top = 243
          Width = 250
          Height = 24
          Caption = 'ImageByString'
          TabOrder = 6
        end
        object Dir: TPanel
          Left = 287
          Top = 141
          Width = 250
          Height = 24
          Caption = 'Dir'
          TabOrder = 7
        end
        object Email: TPanel
          Left = 20
          Top = 90
          Width = 250
          Height = 24
          Caption = 'Email'
          TabOrder = 8
        end
        object Fieldpath: TPanel
          Left = 287
          Top = 396
          Width = 250
          Height = 24
          Caption = 'Fieldpath'
          TabOrder = 9
        end
        object File: TPanel
          Left = 287
          Top = 192
          Width = 250
          Height = 24
          Caption = 'File'
          TabOrder = 10
        end
        object URL: TPanel
          Left = 20
          Top = 141
          Width = 250
          Height = 24
          Caption = 'URL'
          TabOrder = 11
        end
        object Selector: TPanel
          Left = 287
          Top = 294
          Width = 250
          Height = 24
          Caption = 'Selector'
          TabOrder = 12
        end
        object Comport: TPanel
          Left = 287
          Top = 345
          Width = 250
          Height = 24
          Caption = 'Comport'
          TabOrder = 13
        end
      end
      object tsInteger: TTabSheet
        Caption = 'Integer'
        ImageIndex = 1
        object Bevel1: TBevel
          Left = 352
          Top = 13
          Width = 601
          Height = 348
        end
        object IntegerProgress: TPanel
          Left = 399
          Top = 101
          Width = 246
          Height = 24
          Caption = 'IntegerRef?view=progress'
          ShowCaption = False
          TabOrder = 0
        end
        object IntegerGauge: TPanel
          Left = 399
          Top = 160
          Width = 250
          Height = 169
          Caption = 'IntegerRef?view=gauge'
          ShowCaption = False
          TabOrder = 1
        end
        object IntegerFlags: TPanel
          Left = 698
          Top = 160
          Width = 239
          Height = 169
          Caption = 'IntegerRef?view=flags'
          ShowCaption = False
          TabOrder = 2
        end
        object Integer: TPanel
          Left = 27
          Top = 39
          Width = 250
          Height = 24
          Caption = 'Integer'
          TabOrder = 3
        end
        object IntegerInfo: TPanel
          Left = 27
          Top = 106
          Width = 250
          Height = 24
          Caption = 'IntegerInfo'
          TabOrder = 4
        end
        object IntegerSpinner: TPanel
          Left = 27
          Top = 160
          Width = 250
          Height = 24
          Caption = 'IntegerSpinner'
          TabOrder = 5
        end
        object pnl1: TPanel
          Left = 399
          Top = 39
          Width = 250
          Height = 24
          Caption = 'IntegerRef'
          TabOrder = 6
        end
        object pnl2: TPanel
          Left = 675
          Top = 39
          Width = 250
          Height = 24
          Caption = 'IntegerRef?view=info'
          ShowCaption = False
          TabOrder = 7
        end
        object PageControl1: TPageControl
          Left = 27
          Top = 264
          Width = 278
          Height = 121
          Hint = 'IntegerPages'
          ActivePage = TabSheet6
          TabOrder = 8
          object TabSheet1: TTabSheet
            Caption = 'TabSheet1'
            object Label1: TLabel
              Left = 16
              Top = 16
              Width = 17
              Height = 33
              Caption = '0'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -27
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
          object TabSheet2: TTabSheet
            Caption = 'TabSheet2'
            ImageIndex = 1
            object Label2: TLabel
              Left = 24
              Top = 24
              Width = 18
              Height = 35
              Caption = '1'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -29
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
          object TabSheet3: TTabSheet
            Caption = 'TabSheet3'
            ImageIndex = 2
            object Label3: TLabel
              Left = 48
              Top = 16
              Width = 20
              Height = 39
              Caption = '2'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -32
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
          object TabSheet4: TTabSheet
            Caption = 'TabSheet4'
            ImageIndex = 3
            object Label4: TLabel
              Left = 72
              Top = 16
              Width = 22
              Height = 42
              Caption = '3'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -35
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
          object TabSheet5: TTabSheet
            Caption = 'TabSheet5'
            ImageIndex = 4
            object Label5: TLabel
              Left = 128
              Top = 24
              Width = 24
              Height = 45
              Caption = '4'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -37
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
          object TabSheet6: TTabSheet
            Caption = 'TabSheet6'
            ImageIndex = 5
            object Label6: TLabel
              Left = 168
              Top = 3
              Width = 31
              Height = 58
              Caption = '5'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -48
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
        end
        object Panel4: TPanel
          Left = 27
          Top = 208
          Width = 250
          Height = 24
          Caption = 'IntegerPages?view=simple'
          TabOrder = 9
        end
      end
      object tsFloat: TTabSheet
        Caption = 'Float'
        ImageIndex = 2
        object Float: TPanel
          Left = 34
          Top = 56
          Width = 250
          Height = 24
          Caption = 'Float'
          TabOrder = 0
        end
        object Floatcurrencyrate: TPanel
          Left = 34
          Top = 107
          Width = 250
          Height = 24
          Caption = 'Floatcurrencyrate'
          TabOrder = 1
        end
        object Floatinfo: TPanel
          Left = 34
          Top = 158
          Width = 250
          Height = 24
          Caption = 'Floatinfo'
          TabOrder = 2
        end
        object Floatgauge: TPanel
          Left = 370
          Top = 56
          Width = 250
          Height = 126
          Caption = 'Floatgauge'
          TabOrder = 3
        end
      end
      object tsDate: TTabSheet
        Caption = 'Date'
        ImageIndex = 4
        object Date: TPanel
          Left = 67
          Top = 60
          Width = 250
          Height = 24
          Caption = 'Date'
          TabOrder = 0
        end
        object Time: TPanel
          Left = 67
          Top = 111
          Width = 250
          Height = 24
          Caption = 'Time'
          TabOrder = 1
        end
        object Datetime: TPanel
          Left = 67
          Top = 162
          Width = 250
          Height = 24
          Caption = 'Datetime'
          TabOrder = 2
        end
        object Dateinfo: TPanel
          Left = 67
          Top = 218
          Width = 250
          Height = 24
          Caption = 'Dateinfo'
          TabOrder = 3
        end
      end
      object tsOthers: TTabSheet
        Caption = 'Others'
        ImageIndex = 3
        object Currency: TPanel
          Left = 39
          Top = 31
          Width = 250
          Height = 24
          Caption = 'Currency'
          TabOrder = 0
        end
        object Currencyinfo: TPanel
          Left = 39
          Top = 82
          Width = 250
          Height = 24
          Caption = 'Currencyinfo'
          TabOrder = 1
        end
        object Bool: TPanel
          Left = 39
          Top = 133
          Width = 250
          Height = 24
          Caption = 'Bool'
          TabOrder = 2
        end
        object Boolsimple: TPanel
          Left = 39
          Top = 184
          Width = 250
          Height = 24
          Caption = 'Boolsimple'
          TabOrder = 3
        end
        object Boolimagedaction: TPanel
          Left = 39
          Top = 235
          Width = 250
          Height = 24
          Caption = 'Boolimagedaction'
          TabOrder = 4
        end
        object Boolselectedcaption: TPanel
          Left = 39
          Top = 286
          Width = 250
          Height = 24
          Caption = 'Boolselectedcaption?Caption='#1055#1088#1086#1073#1072' '#1087#1077#1088#1072
          TabOrder = 5
        end
        object Color: TPanel
          Left = 39
          Top = 337
          Width = 250
          Height = 24
          Caption = 'Color'
          TabOrder = 6
        end
        object Colorsimple: TPanel
          Left = 39
          Top = 388
          Width = 250
          Height = 24
          Caption = 'Colorsimple'
          TabOrder = 7
        end
      end
      object tsDecor: TTabSheet
        Caption = 'Decorations'
        ImageIndex = 5
        object img1: TImage
          AlignWithMargins = True
          Left = 16
          Top = 339
          Width = 961
          Height = 121
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
          Align = alBottom
          Center = True
          Picture.Data = {
            0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000064
            000000750806000000B868446A0000000473424954080808087C086488000000
            09704859730000071D0000071D016AA943F60000001974455874536F66747761
            7265007777772E696E6B73636170652E6F72679BEE3C1A000017174944415478
            9CED9D799854D5B5F67F6B579D53DDCDA0202868A2C4A8417B0082519478E51A
            150AC428D8A8F76AE2C810639EC42F6AAE4344CD358A4613F493211787241A4D
            031AD02E248A245E3FD404893D0826E88D57028880804D77D739A7F6FAFE6868
            AA9A86AEAAAEEA01799F87A14EEDBDF65BF5D69ED75E5B38889C40275FDACF0F
            F9E355F9A6C2480356B0139CD90B5FCFC44E385F043F0F68985C7EB4187B8141
            2EF4F0CF40090108A08055F355E0A020F944FCBA89C524CC05A017827E1544B4
            F5A46B236EE4C94CED4B7B097E1EE04D2D3F455527805E88C8096D245760BE47
            E2BBBD663FB729D3B20E0AB20F34D5042E052E01F9721A5902E059446744662D
            A8CAB6DC838224A16172F9D1225C88502EE8C834B3C5417FA7CADD057316FCBD
            BD1C3EF782EC9C3C616048429344B804F454D2FF4EB620FA7FDD84FBB0CCFDED
            E65CF1F95C0AA2975FDEC3EFD93009E5328533A169749426FE2EC8438E6D7C52
            E62EAECF35B7CF9520F1C91307237205C2B540DF0CB3AF1465A6B3559E928A8A
            443EF8C1E74010BD3E1AF1821EE78BCA6445CFCE30BB052A8DCA3DCE9C8A15F9
            E0D71207AC20F129E52789D8C98A7C0BE89361F6CF04FE2B417866E1EC67FE91
            077AFBC40125885E1F8D787E8F8B14A6089C91717EF84484996E2478447EFEFC
            B67C706C0B0784208D53261E2FC8D52A5C25D03F0B131B117EEED6153E22BFFE
            F5CE9C13CC00DD56102D2F77BD7EFACD5D7DC337C8EEB37CA02A3323F11E73E4
            89271A73CD311B743B411AAE29FF9209DBC9825CA970447656E46D949FBA038B
            17CAF4E936B70CDB876E23487C727929C6DE087229592E8A2AF23A6AEF8BCC59
            F08234AD397539747941FCC9E55FB786FF008D92255F85D7426A6F73E62CFC53
            8EE9E51C5D56106F6AF9298ADE0E9CD70E3355083F89CC9A5F912B5EF9469713
            A4A969D2BB81F3C9969F528BD1DBDC590B7EDF559BA67DA1CB6C50E9D5E57DE3
            61FB5344AF014C9666B688F26367E0E6B9327D79904B7E1D854EAF210AE24FB9
            E87284FB150ECFD28CAFAA8F47D4BD35972BAF9D814E15A4F1DA8B4F20641FCB
            60EFA115C8DB58FBEF91B90BD6E48E59E721DBA6A1DD884F9D384942893FB74F
            0C7EEB36F61879A088019D5043F4FA68C4F78B1E50E4BBED34B5D2DD22A74B45
            859713625D041DDAA9375C37E118CF0F55807EADBDB61479E24013033A5090F8
            D40B4B34615E05ED971B8B5A921B3B5D0B1DD264E9F5FFDEDBF31B6B40BE984B
            B322FA9093707FDADD4756C9E81041BCA9173DA0F07FF2643E01BCA9502BF091
            42BD11F94CADFD0CA45E8DEE34966D8AA9B712AA2F10B39DCDFECEAEDADCE55D
            102D2F77BDC3740399EF61E71B01F019B003A54E45B6096C07BB4D61BB886C53
            658BC046A3F6A304B2215264D6C943150DF924957741E2532F2C815075BECBC9
            35ACC25FEA2CB1AD01D5F5CA970A84FF1CE4E20A5B41D603EFA1542976552430
            AFCBBC8AADB92837EF827893277C558D5999EF7272015F95D7B65B166F49B078
            6B828D5EEA56C98AA18594F66875EA6605965BE1818259F363EDE190FF26EB8A
            2B0ABC82BAF564EE68D021D86995973FB52CDE1AB0646BC0B67DAC8095F534FC
            A9AC9070DBDFD88CC8ECF93767CBA7633AF529E557ABE82F3BAABCB6B0C55762
            9F062CDA6279755B40431B7B86671C12E28913221CE1A6475F444E76675564D5
            2A74C83CC49D53312F3EEDA21DC07D285FEA88325B6247A0BCF06982F99F042C
            DB96204863517E64EF10377CC161749F4C1C1BC1AA1D09745D410022B3E65728
            CCF7A74D3855D58C1618A9F015E0283273E54C1B710BCBB62758B83960D1E604
            3B6DDB2A0830A66F881B8E7239AD77964B7D42D69E8D9DDE84E8E4C94EA3FDF4
            0BE150E28B89902910E51000513D143162557B891046E921686F457A01BD0539
            1440E1105043531F7578A0F47C757B828A4D012F7C9A60473A5501704498D43F
            C40F8E72185CD4CE3557D121D91E49E874417204090F19335212E652834EB242
            DACB338786E1CA231CA61DE970649A7D441B5811993DFFF46C3377991DC3AC30
            FCEC431CCFB918F82E96524449D7A7E7E888E19A0161AE1AE07068EEBE85C060
            6F6C8F816E2988533A6E38D8C9785C06146592F70857D6DD35C83DF2E27E6193
            C6103623A8CACDCE9CCC4EDDB644F769B28E8BF6760AB904E43BA04332CC6D45
            A8B46A7F1154BFF4B23775E2B58ACCCD213B0BFC4764F6FC19ED35D4E50571CA
            A2235099023A890C6B0310077E65AC9D11AF7D696DF21BDED48B5E5518D55E7E
            0A9F805E5E307BC14BEDB5055DB5C9FACAF9BD1CD7BF14988A322C0B4F9E9D88
            CC0BA3F73754C5D6B596C0AA2C16D151EDA129B02C817CAB68F6FC7FB6C74E32
            BA9420CD7D03FEBF013DB330B1436096E77BF7B3E6952DFE7E128AD17EEDF0D8
            DAAE2A77B8038B1F7673EC1BDCF94DD6A051056EAF82F18A994CE6279C76E363
            1199ED39DE43AC7C797B5B891BA74D3C16953785F487C7BB9000FD7510766EED
            F1C833EBB3E4BA5F745E0D291E3BC035FA7D85690ABDB374305C0BCCF01BF815
            6B2BE3E964689C36F16C51F92F321343810598D08F238F3EBB3A1BA2E9A2C36B
            4841E9B863136A7F8870255090A599BF0972A737B8C7B3A4790053A7FD5B1F4F
            BD07802BC9E4730B4B04B92DDBC5C24CD16182140C1D3328913077805E46B635
            53F810CB5D7EBF865FB13C3D57519D3EDDF81B6A2E439891D9791279C39AC42D
            858F2E7C352BAE5922EF82140D3BE748DF776EDDE5B3EB6663436083C23DBEED
            3997DAF4F7C21BA6969F65E0FEA62031696395AADE56306741651654DB8DFC09
            327C7C911B0F6E51E106A0304B2B0D020F7892B88FAAA5699FFDF3BF73D1E96A
            B94BE11BE917256F237AAF3B6BFEFCCEF498CF8B2091B2E878ABF20BD0ACF73E
            4478C190F85E63D5D2FF49378F376DC20855732B199C29D97DAAAA60CE82C559
            11CD31722A4861F1F8A37D133C2C4D673BB245AD5AA604B5B1B4D784FC29E5A7
            59E136D0B1696651609120F7B8B32BDECA8E667E903341422563CF33A24FD22E
            771FF9AD2FC1B5E9364F4DC7DDF466D2AF1116A8146BEF70E72E7C3B6B9A7944
            FB0519352AEC6E29FC89C24DEDB127E8126F70AFF3DA1AC6EAF451616F63BF8B
            692AAF2C4DF33B4479CC1A7DB860D6820FB2E5D81168972045C3CE39D20F9C67
            40338E9AB01754BEE6D754FE659F6F5F7145815FB0F34AC5DE0432284DAB1FA8
            F070241C794C1E7E6A47BB397600B29EA93BA5E38607817D0174404E98186D95
            8B5E7E798F7851E3359ED4DD081C95E66FA82972CFC0CD4F77B7A36D59D590C2
            9273BF1848E82F647F04AD35D48A6592571B7B1740AF3ABF57DC895C25A23F02
            D211DD037E2F621F74672D7C2387BC3A145909122E8D3E277041AEC900418191
            1513FB99C4E83EE1AF0D88488F22231C1A128A425020D07BEF6DBE0F417EE91B
            E6F57CB462631E3875283216245212FDB215FE9E4DDE5CA15708C2423C6EF9B4
            C1B241857A947A906DC04E841DA2BACD22DB45759B18D96EB1DB8C9A4D5E5837
            B22AF64967716F0B19F721D6702EDAB9CBF69F358DC32234356503F6CCABB5F9
            1F0504050155451014C50980D26823B01E653DB04E0CEB54E55D902ABFC1D6B0
            3696D6CA713E90B120AA7A9874816D9476A2003816E1580055689250710A69A4
            34FAFF80677CDBF3C94CD6CE72818C3DC28C529B0F225D0805C059C05C47EA6A
            22C5A38FEBC8C23316C4EBD7B818C83A5070B78270BC35E67774607F99B9CFE4
            F2E5816F6534925990F96E8C6191E2D1E944B6CE09B27362ADADDCE857C5BE8E
            C808849F01AB207B07E3AE0E15C9D4FD286BE4AE2A9E565EE87E563F28217690
            110689E52884B0A23D411C440B5153A0684490A2A6D7F446E88DD21BE84D9EBC
            E0DB893ADFDDD497952BF7E7C4923374BDE1D271D10885A6A800FA2444FB5B9B
            E8674C6820AAC52827239C0A381DC8E849BF3A76454715D6F504690BC5A37A46
            42056759958B818BC96FAD6A30A1D090F85F5F687790FD74D1FD044982533C7A
            08C63C0D9C94970294ABFD9AD86379B1BD0F746B4100281DD7C711BB0C65680E
            ADAA2A370535B1077268332D747F4100B764DC892AF61D72D3B7EC10956BBD9A
            CADFE5C056C6E8B47859B98457F3E26A90F62FB9ABBC1592C4D0CE12030E1041
            9AA0ED59736A50F891DFAF7E64265E2EF94097F27ECF16EE90D1C56AC92A329D
            C05231727DFC9DCABFE59A5736E8F6823825634F56AB1564EE27BC42556FF16B
            962CCF03ADACD17D3BF5D3CA0BDDBABA3B147E486673915A41EFF4AA97CCA70B
            C6F4ED968244CAC69C6F551E04D25FF453FE2E46EFF0AA463C0B5D2B007F32BA
            95206E71F4240C0F299C9B41B68F80BBFDC31A1E4FD763BE33D12D04890C3DEF
            789BB03F06BD94F49BA77F207ABFBFA3F131FEB1BC4BDC0D920EBAB42005278E
            3D2611D65B80AB487F00F23E30C377373DDE512BB4B9449714649710B7025790
            FEECFB6D11B9C7AB3AE5B9AEDC47B4852E25885B36AE4449DC84CA25A42B84F0
            BA81FBE255B117E882A3A64CD12504090F19F375A372B32AE3488F93555884D8
            0783AA975ECB37BF8E44670A22A192B1E38CD11FA169CFB23F43E471637566BC
            26F67E5ED975123A4190E92652F6E638ABDC0EA419725CFE47D13901662ED52F
            7E9A577A9D8C0E1464BA714BDF98A8C874D2DF505A8930D3EFDBF074779843E4
            02F91764D0A802A757C1B7416E24BD99753DF00C9847FDEA17BB4578D95C227F
            820C8BF60F074C13B88EB48E2DC81A84D9BE719FE4AF9D73ED695740EE05292E
            77DDD067D355E5FBB47D1CDA037D5E955941CD923F72000C5BDB8B9C2FBF3B66
            E73C55B96CBF89840F15E606D69947CDA28F73CDA13B23B73564F8F87E8E176C
            6ACDAEC27A419F531B5A10D47EED8FDD79369D4FE4B88634F8E0D401BD000FE1
            CF62F9A3552A83DA5357EC11E1C5DC167B0021F77D48E9B83E9190F48B1716AE
            63457EAF763888833888CF1BBAC4E2E20185E272D73175DF1674822283155C41
            DEB7D87B13D54BDA0CF9D4EDBD4EBA14868FEFE7787531E0E4DDE7629BFED623
            8D4A5AA1EF0E2047B9CE87EB259E004E6EF54D435A6B71076B488EE0968C3B51
            B1E3929F35CDBD5820506F559A0ECB968EEBE3605362BA840239AB7175E58770
            50909C41C57EBDC5A3BA20B027B3FAA50D294F5D13C2B3C7A6E43589E6DDD183
            4D568EA0D2720155ABF612230DEC5D434E2B2F74EA77945A6B0E13AC6FD0755E
            F5E97F4B7BA9A3BC3C54F0DEF6A3036B8EC110326AFFE9552F7D8F2C160E2343
            CF3B3E1124BE8421087CBB3ABD0F38DDB8C56F0EB6224761088965B35F575F93
            892B50D1B0738EF47C7710A1442F49982D7EA356B719DDC16A18491EB49AFA74
            CB4B46B3855D615CEF019D4053D88A646C4178C237919FEC7369FCB868245CA4
            DF1395EF008392DF52F827CACF03ED39B3656404A724FA1A42737557B1FF8248
            9158F919509C4A565EF4347C75AB0B92BB836EC214F6BED0C5535864D4FCB8E9
            E842EBDF855332E62A8C7CB795C33FF5882E3056EE6CB975EC948E5D0D3A781F
            3677E31F287723CC6B23DD6A01081747478AE14568BA6E687F868D72F65EFBD9
            C5630738461702A7ED3FBBBCECBBA16FB27271F3AF672F4160BEC004F6DD9CAE
            F2DD4DA7A6F85C0D8BF677025EA6ED087371412EF6AA2B7F9FCA7F54CFB029FC
            751A118E7658B824511D6BBEAB30D782188A47F5C4F03B52C5D82AB014781352
            2EAD196485674899504E378E68056D8A01A0673B5EF0F3FDA510B888FDF76DC3
            5CBFFFC4E407619F59A48AE1037F42F4559AAE57DD8D88A2BF292C1E7F74727E
            2754F0689AE1A67A1B98EF0C890E4B236D56306EA8E03C8123F73C9235BEEB1F
            EB55C746FBD5B111161D4F6AFB7FB25336F694DD2F9CB237A724FFC2818428FF
            29624AB176A8C07DA48A7A75A46CCC57F6C349517E81B54345CD49A053819DA9
            09D8137DB4ECDCC345B830E96D1FB123FCEAD8997ED592B3C2C24902C971B47A
            FAC6FFD6EE17E192E8B9A85CDE82C3535819160A6410E895C096A4F78A501E6E
            2EAC414FF5317D05B9B7858D3FFA98BE3EA6AFEFFA43FD469EF2317D8DB5C7EF
            F589C50EF7317DFD0646842D726CEAFA89AE48BE612051BDA4D294469F06690E
            D3AD96DE7B92CB9464BD44E541AFA6F2B62483EF38A5D1FE34B9830218552E03
            6EDF8B188030C7AF8E7D3FE9C96AA774CC092037343FB1A6595027618EC1ECA9
            510A1F07552F35471C6DA88AAD734AC7CC00931C42B6B9B913916B5A8C37FEE0
            57C72E077457A227C26563378AEA9E2B559591CE90E830FF9DD82AD6C67600D8
            92318D92D2A98BDF8A874C3C3E7C7CC8F152E78826C10E6A9BD28645757D8B25
            AD494E5974B95FCFB3BB47167E75ACD51DC048F1E8E36C8BEB87421A7AA4653A
            517D4145760B8222FB6CDED4D28ABF95A949FDD2B4F91AD770483604BA2B3C56
            D35F5F704BC6CEF022A119AC5CBCB989FF92878087F6323B6A54982D9A1262D6
            AACCA485424155E512A7345A4BD22043133286A690223945D80F153CEF24E2F7
            B1C711A107CA934E21B3A434FADF169689CA2B7ECD296FB71CFA2642A1E34453
            B90726F180539A1ACF58C5B6BC3CE50B999014D4D3D407CD35A2A12AB6CE2D1D
            5BA968F32C59456F74BCE0064AA3AB049659E5952012FEEFE4C10440E1D6C201
            418B7D7FC704B5FB5873FA0B498248DB1D795630FCF5F96D08DF045A0E258B14
            CE15B817D13F3BA56FD5BA25632624271065608B3C61D0F2BDFEA8FC6B4ABEB6
            477319C1F3E3DF065A9EC20D01272BDC24C24B8E177C142E8DDECEF0E1CDB3E2
            C0CA5EC1351B7B1CD26ADC46115D9FFA9AFE39A0BE170C805F157BC3F7BD6251
            EE01F611685807ABC87CA774CC0FF63CB259CDF435D72B046B5ED9E21FD67006
            301995B7A0D5EB0CFB0ADCE57A473C07D3CD2E227B2FF8798DAD6E49A84ACA15
            4CB67DA77EF7893D33F535AF6CF1E056E056B7387A9215A206CE51E11CF67C81
            02724F6159B4A2A12AB64EC46CD6D4B6A801917F4565FF2B9B36C8BD17E2F2E5
            810FBF047E49C9F94738128C05FD064D71E87BED4EA6E838B7F4AD4BBC6A9E0E
            877473CB9B5923093D2A0E7BC536511898D2652B7909A4190E9745CF4AFEA507
            837BBFEA5554BC0BBC0BFCCC2D1B57A2D6BE9A34FB2D483445B2FE6DC8865605
            26E5BB2D54AB870435954B5B2D6DF87027D787689CE2B143D5249A67E641D8BC
            C3AA451FFBF038F038432F38D449788B92A36F2B9C033CDDD4FF44376A525C60
            6B83D3692948797988357523529E09FB8CC2DD1E1851E609E60FBBFF38EFED4C
            59CFF7AA5EAC1121E50601DD75317043EDE2FF055282DA8B616E61C9B95F6C59
            50B86CF4198E77F8BB6EC9B81373FA09C45E97CC3F1CE89494F7FFFAFC36B0CF
            A5F2D743F7FC9FD46B2A546EA3745C9FE447CE7B75D74BEA402408ABCDEEC297
            466FAFE63421E1E685C930228B50FDDE1E42FA64B86CCC75C18EC6D7E9ED865C
            0D5DD0CAC528CDC33D234CB7CAA23DF9392690D06AA774EC6F50AD55A1C8C099
            AA8C01448D8D150D3BE7F4FA557FC8C92D67C6C822AB5CB3FBB520B73825D1F5
            BE98E7B0453B9DD0CE61A85EB72FFE42E24125F46DF6DCFE739C23769596461F
            13640B7026AA17A56697DF34D42CFD282BC227F5DECE9A3A2FA93C44EC9C7069
            B4C228AEF16DF81EE07F93B27C45545E767A1536381AAA53F80D498B8D024BFD
            AA58F388265E155B0CFA608B627B804E41982970AF4294E6DD4C067A8970CE86
            8CF1AAD862D5945F7921C23C07BBD531757154DF20D5C97B532089D9BB5F78D5
            4BD72072538A51E518813B411F691A29A64CD4DEF743EE0FC816151509903FB5
            785A2270A70AB71A6A167D6CACFD062D9A9ED62050E9B9FEA496CFFDEA253F54
            B899A6AB4EF70985F5162E08AA62CB32FA106D20E8D5F362E0A93492AE15E46C
            AA966E4A7EE85755FE02B89E36F8036F3AE1E05FDAED0C2E7AFBBECA0A01243E
            797FAB3D73D8BCF066EF6F0A22D083A65A1147F910A395AA72935F13BB9B0D1F
            B46AC86E5AFB7A64C0F14FA8D200520044105C850D027F061E0A1A98AC6B6229
            717F43471C5F065207F201C80746745962D3DA7753F80F38BEBF2047ED4E83F2
            AEDDB4F6F9E604EBDE0DECA6B50BCD802F2F13C407D9CD1F858F0DF21ACA7D7E
            23D3126B62AD369576D3DAB722FD4FFC958AF569BA73B710300A1B80578CE85D
            7EF5881BFC8D8FB57AED45E8F0138E46A4D76E8E82BE9DD8B4F6E556CBFA78ED
            BAD0E1272C51D12304396C57599B4156FC7FE0ED0FAF5A847193000000004945
            4E44AE426082}
          Proportional = True
          Stretch = True
          Transparent = True
          ExplicitLeft = 32
          ExplicitTop = 176
          ExplicitWidth = 137
        end
        object spl1: TSplitter
          Left = 0
          Top = 315
          Width = 993
          Height = 8
          Cursor = crVSplit
          Align = alBottom
          ExplicitTop = 543
          ExplicitWidth = 1073
        end
        object Splitter1: TSplitter
          Left = 0
          Top = 105
          Width = 993
          Height = 8
          Cursor = crVSplit
          Align = alTop
          ExplicitTop = 73
          ExplicitWidth = 1073
        end
        object Panel1: TPanel
          AlignWithMargins = True
          Left = 16
          Top = 16
          Width = 961
          Height = 73
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
          Align = alTop
          TabOrder = 0
          object lbl1: TLabel
            Left = 32
            Top = 25
            Width = 333
            Height = 16
            Caption = 'TLabel (Tahoma,10,[],DEFAULT_CHARSET,clWindowText)'
          end
          object lbl2: TLabel
            Left = 400
            Top = 16
            Width = 446
            Height = 25
            Caption = 'TLabel (Segoe UI,14,[B],RUSSIAN_CHARSET,clRed)'
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clRed
            Font.Height = -19
            Font.Name = 'Segoe UI'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object Panel3: TPanel
          Left = 0
          Top = 113
          Width = 993
          Height = 202
          Align = alClient
          TabOrder = 1
          object shp1: TShape
            AlignWithMargins = True
            Left = 784
            Top = 17
            Width = 192
            Height = 168
            Margins.Left = 16
            Margins.Top = 16
            Margins.Right = 16
            Margins.Bottom = 16
            Align = alRight
            Brush.Color = clMoneyGreen
            Pen.Color = clTeal
            Pen.Width = 3
            Shape = stRoundRect
            ExplicitLeft = 880
            ExplicitTop = 1
            ExplicitHeight = 462
          end
          object bvl1: TBevel
            AlignWithMargins = True
            Left = 17
            Top = 17
            Width = 200
            Height = 168
            Margins.Left = 16
            Margins.Top = 16
            Margins.Right = 16
            Margins.Bottom = 16
            Align = alLeft
            Shape = bsFrame
            Style = bsRaised
            ExplicitLeft = 1
            ExplicitTop = 1
            ExplicitHeight = 462
          end
          object Splitter2: TSplitter
            Left = 233
            Top = 1
            Width = 8
            Height = 200
            ExplicitLeft = 201
            ExplicitHeight = 462
          end
          object Splitter3: TSplitter
            Left = 760
            Top = 1
            Width = 8
            Height = 200
            Align = alRight
            ExplicitLeft = 877
            ExplicitHeight = 462
          end
          object Panel2: TPanel
            Left = 241
            Top = 1
            Width = 519
            Height = 200
            Align = alClient
            Caption = 'Panel2'
            TabOrder = 0
          end
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Scene'
        ImageIndex = 6
        DesignSize = (
          993
          476)
        object Panel7: TPanel
          Left = 3
          Top = 3
          Width = 987
          Height = 470
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'SimpleChart'
          TabOrder = 0
        end
      end
    end
  end
end
