*****************************************************************
*  System      : ERP项目
*  Module      :  PP
*  Program ID  :ZPPR024
*  Program     :  销售订单BOM批量查询
*  Author      :  
*  Date        :  
*  Description   :  ****
*****************************************************************
*  Modified Recorder :
*  Date              C#NO                    Author                Content
*  --------------------------------------------------------------------*
*  修改日期           C票或变更文档ID         修改者                 修改内容
*  或修改的传输请求号
*****************************************************************
*

*&---------------------------------------------------------------------*
REPORT zppr024.
TABLES:kdst,mast,stko.
*&---------------------------------------------------------------------*
*   TYPES定义
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF typ_alv,
    SEL     TYPE C,
    werks   TYPE kdst-werks,  "工厂
    matnr   TYPE kdst-matnr,  "母件编码
    bmeng   TYPE stko-bmeng,  "BOM基本数量
    bmein   TYPE stko-bmein,  "母件物料基本单位
    maktx   TYPE makt-maktx,  "母件物料编码描述
    stlan   TYPE mast-stlan,  "BOM用途
    stlty   TYPE stko-stlty,  "物料清单类别
    datuv   TYPE stko-datuv,  "有效生效日期
    postp   TYPE stpo-postp,  "项目类别
    posnr   TYPE stpo-posnr,  "行项目号
    idnrk   TYPE stpo-idnrk,  "组件物料
    idnrk_t TYPE makt-maktx,  "组件物料描述
    menge   TYPE stpo-menge,  "组件数量
    meins   TYPE stpo-meins,  "组件物料基本单位
    potx1   TYPE stpo-potx1,  "行项目文本1
    potx2   TYPE stpo-potx2,  "行项目文本2
    lgort   TYPE stpo-lgort,  "生产仓储地点
    sanka   TYPE stpo-sanka,  "成本核算相关
    SORTF   TYPE stpo-SORTF,  "排序字符串
    vbeln   TYPE kdst-vbeln,  "销售订单
    vbpos   TYPE kdst-vbpos,  "销售订单项目
  END OF typ_alv.
*&---------------------------------------------------------------------*
*   CONSTANTS定义
*&---------------------------------------------------------------------*
CONSTANTS:
  gc_success TYPE icon_d VALUE icon_green_light,
  gc_error   TYPE icon_d VALUE icon_red_light,
  gc_await   TYPE icon_d VALUE icon_yellow_light.
*&---------------------------------------------------------------------*
*   DATA定义
*&---------------------------------------------------------------------*
DATA:
  gt_alv   TYPE STANDARD TABLE OF typ_alv,
  gs_lyout TYPE lvc_s_layo,
  gt_fact  TYPE lvc_t_fcat.
*&---------------------------------------------------------------------*
*   SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS:
    s_werks  FOR mast-werks MODIF ID m1 OBLIGATORY,      "工厂
    s_matnr  FOR mast-matnr MODIF ID m1,                 "物料编码
    s_stlal  FOR mast-stlal DEFAULT '1' MODIF ID m1.     "物料备选清单
  PARAMETERS:
    p_stlan TYPE kdst-stlan DEFAULT '1' OBLIGATORY,      "物料用途
    p_datuv TYPE stko-datuv DEFAULT sy-datum OBLIGATORY. "有效开始日期
  PARAMETERS: p_c1 AS CHECKBOX DEFAULT 'X'. "多层
SELECTION-SCREEN END OF BLOCK blk1.
*&---------------------------------------------------------------------*
*   START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

* 权限卡控
  PERFORM frm_check_authority.
*&---------------------------------------------------------------------*
*   END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
* 主处理
  PERFORM frm_ready_report.
* 设置样式
  PERFORM frm_pre_fieldcat.
* 设置布局
  PERFORM frm_set_layout.
* 报表展示
  PERFORM frm_show_alv.
*&---------------------------------------------------------------------*
*& Form frm_ready_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_ready_report .

* 获取数据
  PERFORM frm_get_data.

* 数据编辑
  PERFORM frm_deal_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .

  DATA:
    ls_alv TYPE typ_alv,
    lt_stb TYPE STANDARD TABLE OF stpox.

* 销售BOM数据
  SELECT A~matnr,
         A~stlal,
         A~stlan,
         A~werks,
         A~vbeln,
         A~stlnr,
         A~vbpos,
         B~maktx
    FROM kdst AS A
   INNER JOIN MAKT AS B
      ON A~MATNR = B~MATNR
     AND B~SPRAS = @SY-LANGU
   WHERE A~werks IN @s_werks
     AND A~matnr IN @s_matnr
     AND A~stlal IN @s_stlal
     AND A~stlan = @p_stlan
    INTO TABLE @DATA(lt_kdst).

  LOOP AT lt_kdst INTO DATA(ls_kdst).

*   BOM展开
    CALL FUNCTION 'CS_BOM_EXPL_KND_V1'
      EXPORTING
        capid                 = 'PP01'
        datuv                 = p_datuv
        ehndl                 = '1'
        emeng                 = '100'
        mktls                 = 'X'
        mehrs                 = p_c1
        mtnrv                 = ls_kdst-matnr
        stlal                 = ls_kdst-stlal
        stlan                 = ls_kdst-stlan
        svwvo                 = 'X'
        werks                 = ls_kdst-werks
        vbeln                 = ls_kdst-vbeln
        vbpos                 = ls_kdst-vbpos
      TABLES
        stb                   = lt_stb
*       MATCAT                =
      EXCEPTIONS
        alt_not_found         = 1
        call_invalid          = 2
        material_not_found    = 3
        missing_authorization = 4
        no_bom_found          = 5
        no_plant_data         = 6
        no_suitable_bom_found = 7
        conversion_error      = 8
        OTHERS                = 9.

    ls_alv-werks = ls_kdst-werks.
    ls_alv-matnr = ls_kdst-matnr.
    ls_alv-vbeln = ls_kdst-vbeln.
    ls_alv-vbpos = ls_kdst-vbpos.
    ls_alv-stlan = ls_kdst-stlan.
    ls_alv-MAKTX = ls_kdst-MAKTX.

*   取STKO表数据
    SELECT SINGLE
           bmeng,
           bmein,
           datuv,
           stlty
      FROM stko AS A
     WHERE stlty = 'K'
       AND stlnr = @ls_kdst-stlnr
       AND stlal = @ls_kdst-stlal
      INTO ( @ls_alv-bmeng,@ls_alv-bmein,@ls_alv-datuv,@ls_alv-stlty ).

    LOOP AT lt_stb INTO DATA(ls_stb).
      ls_alv-postp   = ls_stb-postp.  "项目类别
      ls_alv-posnr   = ls_stb-posnr.  "行项目号
      ls_alv-idnrk   = ls_stb-idnrk.  "组件物料
      ls_alv-idnrk_t = ls_stb-ojtxp.  "组件物料描述
      ls_alv-menge   = ls_stb-menge.  "组件数量
      ls_alv-meins   = ls_stb-meins.  "组件物料基本单位
      ls_alv-potx1   = ls_stb-potx1.  "行项目文本1
      ls_alv-potx2   = ls_stb-potx2.  "行项目文本2
      ls_alv-lgort   = ls_stb-lgort.  "生产仓储地点
      ls_alv-sanka   = ls_stb-sanka.  "成本核算相关
      ls_alv-SORTF   = ls_stb-SORTF.  "排序字符串

      APPEND ls_alv TO gt_alv.
    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_deal_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_deal_data .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_pre_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_pre_fieldcat .

  DATA ls_fact TYPE lvc_s_fcat.

  DEFINE set_fact.
    CLEAR ls_fact.
    ls_fact-fieldname  = &1.   "字段
    ls_fact-coltext    = &2.   "列标题
    ls_fact-ref_table  = &3.   "输出数据的内表名
    ls_fact-ref_field  = &4.   "输出数据的字段名
    ls_fact-checkbox   = &5.   "复选框
    ls_fact-edit       = &6.   "可编辑.
    ls_fact-f4availabl = &7.   "F4
    ls_fact-outputlen  = &8.   "输出长度
    ls_fact-key        = &9.   "显示为主健
    ls_fact-no_zero   = ''.    "为输出隐藏零
    APPEND ls_fact TO gt_fact.
  END-OF-DEFINITION.

  set_fact:
    'WERKS' '工厂'(S01) 'KDST' 'WERKS' '' '' '' '' '',
    'MATNR' '母件编码'(S02) 'KDST' 'MATNR' '' '' '' '' '',
    'BMENG' 'BOM基本数量'(S03) '' '' '' '' '' '' '',
    'BMEIN' '母件物料基本单位'(S04) 'STKO' 'BMEIN' '' '' '' '' '',
    'MAKTX' '母件物料编码描述'(S05) '' '' '' '' '' '' '',
    'STLAN' 'BOM用途'(S06) 'KDST' 'STLAN' '' '' '' '' '',
    'STLTY' '物料清单类别'(S07) 'STKO' 'STLTY' '' '' '' '' '',
    'DATUV' '有效生效日期'(S08) '' '' '' '' '' '' '',
    'POSTP' '项目类别'(S09) 'STPO' 'POSTP' '' '' '' '' '',
    'POSNR' '行项目号'(S10) '' '' '' '' '' '' '',
    'IDNRK' '组件物料'(S11) 'STPO' 'IDNRK' '' '' '' '' '',
    'IDNRK_T' '组件物料描述'(S12) '' '' '' '' '' '' '',
    'MENGE' '组件数量'(S13) '' '' '' '' '' '' '',
    'MEINS' '组件物料基本单位'(S14) '' '' '' '' '' '' '',
    'POTX1' '行项目文本1'(S15) '' '' '' '' '' '' '',
    'POTX2' '行项目文本2'(S16) '' '' '' '' '' '' '',
    'LGORT' '生产仓储地点'(S17) '' '' '' '' '' '' '',
    'SANKA' '成本核算相关'(S18) '' '' '' '' '' '' '',
    'SORTF' '排序字符串'(S23) '' '' '' '' '' '' '',
    'VBELN' '销售订单'(S19) '' '' '' '' '' '' '',
    'VBPOS' '销售订单项目'(S20) '' '' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_layout .

  gs_lyout = VALUE #( zebra      = abap_on    "斑马线
                      cwidth_opt = abap_on   "自适应
                      box_fname   = 'SEL' ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_show_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_alv .

* 报表展示
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_pf_status_set = 'FRM_ALV_STATUS'
*     i_callback_user_command  = 'FRM_USER_COMMAND'
      is_layout_lvc      = gs_lyout
      it_fieldcat_lvc    = gt_fact
      i_save             = 'A'
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_check_authority
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_authority.

  SELECT *
    FROM t001w
   WHERE werks IN @s_werks
    INTO TABLE @DATA(lt_werks).

 CLEAR s_werks[].

* 工厂权限校验
  LOOP AT lt_werks INTO DATA(ls_werks).
    AUTHORITY-CHECK OBJECT 'C_FVER_WRK'
     ID 'WERKS' FIELD ls_werks-werks.
    IF sy-subrc = 0.
      s_werks = 'IEQ'.
      s_werks-low = ls_werks-werks.
      APPEND s_werks.
    ENDIF.
  ENDLOOP.

  IF s_werks[] IS INITIAL .
    MESSAGE '没有工厂权限'(c01) TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
