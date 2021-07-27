package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class NormalizeSimpleTest {

    @Test
    public void getHeaderArray_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "exploded", "-972721203")
            .header( "nonEmpty", "-907286509,-934009414,-677667374")
            .header( "nullableNonEmpty", "")
            .header( "empty", "-701771759")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_EmptyItemsSize_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "exploded", ",-494017518,1060074855,149430002,377055373")
            .header( "nonEmpty", ",874225736,-314269724,841955969,-794630357,-314269724")
            .header( "nullableNonEmpty", "")
            .header( "empty", ",608361895,-502743754,465594649,-1045547692,127171701,637383328,-962858645,986205292,1023639635")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "-65348079")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-46311360,378511667")
            .header( "nullableNonEmpty", "-503095293,-532878902,-318562414,280798610,-603658663,-98320100,519552822,462182147,57243451,-512294150,-1051641101,6100939,-945500279,-530402359,336499925,-772049311")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", ",-1072776539,-422145851,-305459000,-425915437,981451665,50917333,-191279345,400803909")
            .header( "exploded", "849073705,985736034,985736034,-501384396,668558721,-889047644,-128303171,534366710,-217565594")
            .header( "nonEmpty", "818543089,667821749,-535501089,777657227,-249156046,890747956,137755239,938316730,-798265532,464061724,818543089,744638460,-823095573,235672712,-958632106")
            .header( "nullableNonEmpty", "")
            .header( "empty", "114926030,1030595854,-408266226,-408266226,743783680,78684596,-933155123,343130644,881988627")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0")
            .header( "exploded", "-363851236")
            .header( "nonEmpty", "-604125131,-746270766,-1028281162")
            .header( "nullableNonEmpty", ",469615418,-610677691,-467341697,-911646425,469615418,101716121,427927914")
            .header( "empty", "-460853349")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "223453971,-395179547,223453971,-156732865,-536224816,-604370114,-795162578,-464615612,372669629,427891459,619828490")
            .header( "exploded", "0,-744863277,-869859539,-759412114")
            .header( "nonEmpty", "0,-649774598,859805674,-78928759,251035947,435622284,263617608,853776598,559338153,1058144316,-695552803,-1069390196,2840475,-545797367,0,357276767")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0,925691103,20413021,27827168,-938233287,689286766,-228186011,495270445")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "exploded", "283604822")
            .header( "nonEmpty", "266084572,23741015,-736839981")
            .header( "nullableNonEmpty", "0,-247254367,-802268599,994659888,91816500,-406511690,-24446909,-966322417,-719116997,-411900912,314307630,-169690451,-862064725,-549577798,-383501076")
            .header( "empty", "858743657")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "-196028692")
            .header( "exploded", "-599409023,837090590,912829613,-193059441,960248174,-391644768,-664591469,960108518,837090590,-879670742,-312296466")
            .header( "nonEmpty", "-235570526,-591057632,671787927,348512206,-553849820,1043270797,375210383,371026107,371026107,-765458501,-481820307")
            .header( "nullableNonEmpty", "235873520,673340047,356308722,428555860,-491696669,428555860,-516902796,1065509238")
            .header( "empty", "-1022247309,852818677,-28500728,556111612,-642932060,-391222419,-620388289,-620388289,-244296031,341782342")
        .when()
            .request( "GET", "/header/array")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderArray_EmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,366828100,574597416,-997367637,-835520343,-481810369,83827352,-196453495,1056473377,189113807")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-929351801,368321761")
            .header( "nullableNonEmpty", "")
        .when()
            .request( "GET", "/header/array")
        .then()
            // empty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,571079060,7747961,308580898,-415703687,834517023,-164059646,327027580,-705657945,-593146751,666684806,-869609345,858467162,-549314706,-15453159")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-616715181,2987820")
            .header( "nullableNonEmpty", "")
            .header( "empty", "")
        .when()
            .request( "GET", "/header/array")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_EmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,564618885,792163214,-447601450,-62772898,196533609,-409975371,394611238,16390984,269201534,-254202797,1065332495,388802110,-493814795,-881176145")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,1030133466,326614669")
            .header( "nullableNonEmpty", "")
            .header( "empty", "true")
        .when()
            .request( "GET", "/header/array")
        .then()
            // empty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,498747587,435110477,-280746219,453577504,-586190121,906026544,360786863,676190970,-676824170,380882877,601287391")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-483231528,368836029")
            .header( "nullableNonEmpty", "")
            .header( "empty", "~e$zo),1<}EcV")
        .when()
            .request( "GET", "/header/array")
        .then()
            // empty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-1027863008,592064701,-964951122,269627968,560550351,95169914,844580060")
            .header( "exploded", "0")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-1063013721,-291847727,-1047041194,539519531")
            .header( "exploded", "0")
            .header( "nonEmpty", "")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-378662112,344779030,-939571018,847835494,148502260,851551388,797342")
            .header( "exploded", "0")
            .header( "nonEmpty", "-844")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nonEmpty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NonEmptyItemsSize_Is_2() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,1055907271,374464309,-564357895,206029866,-654805198,-482200664,-149895798,611382756,-737233764,685265897,887488807,-36725961,663524932,356458948,763989996")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,0")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nonEmpty.Items.Size=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NonEmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-8001765,299410697,685066164,-274174771,-923486513,851460255,132884775,271203410,-777085721,66421106")
            .header( "exploded", "0")
            .header( "nonEmpty", "-730.4,420140716,-205806014")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nonEmpty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "exploded", "0")
            .header( "nonEmpty", "0,1032923040,326972099")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NullableType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "441.1")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,107231058,264743228")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nullable.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NullableItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "_nS0,47282744,998551810,121574541,138665346,33025194")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-80852629,-549948936")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nullable.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NullableNonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-242463801,554586072")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,1008277407,-269523562")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nullableNonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NullableNonEmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-365628892,-382286127,-98327974,-427792747,-608809848,-149437755,-427597154,265071048,958665101,547484172,780143090")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-763893747,485455788")
            .header( "nullableNonEmpty", "true")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nullableNonEmpty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-903570376,-628218236,975664176,997270629,-169355105,-219251274")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-90913350,2923518")
            .header( "nullableNonEmpty", ",-238831859,864930298,-185350091,-620393586,-1008737065,-938107645,-834707658,446470733,427125643,994770258,704136975,-810193411,831302184,-345606830,-520930544")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nullableNonEmpty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_ExplodedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,709709580,-194637413,-332258874,130493687,598879515,-153428007,515514166,361106640,-221447800,-360957493,92688565,-462925507,829152323,354254439")
            .header( "nonEmpty", "0,-103535513,-338016944")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // exploded.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-1064633573,-828629422,-515984222,679335647,236775215,777559326,-372647293,-1064682308,-778341562")
            .header( "exploded", "")
            .header( "nonEmpty", "0,1005031531,57070155")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-730001660")
            .header( "exploded", "true")
            .header( "nonEmpty", "0,213547727,413617300")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // exploded.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_ExplodedItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "0,-1033306677,-1042846381,241417734,-538521063,-830742257,-259852221,655376736,243081696,500478927,784867120")
            .header( "exploded", "true")
            .header( "nonEmpty", "0,-357455006,160080599")
            .header( "nullableNonEmpty", "")
            .header( "empty", "0")
        .when()
            .request( "GET", "/header/array")
        .then()
            // exploded.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "exploded", "")
            .header( "nonEmpty", "")
        .when()
            .request( "GET", "/header/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "exploded", "width=0,height=0,vpeqmisdaaysqh=&,duuthppakwkxfp=")
            .header( "nonEmpty", "\"width,0,height,0,plup,jlwryyhqnzfzjvk,true,diaspjujihlpr,true,vkuusefizgzi,Chqkd>?,_2_,HS \"")
        .when()
            .request( "GET", "/header/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0,height,0,umuqktmzephlrv,true")
            .header( "exploded", "width=")
            .header( "nonEmpty", "width,")
        .when()
            .request( "GET", "/header/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,")
            .header( "exploded", "height=,atjetpmiqqfjgj=,sgvdwazxltwi=true")
            .header( "nonEmpty", "width,921716868,height,,ro,-772.4")
        .when()
            .request( "GET", "/header/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "height,,avlctbawgnkdo,3")
            .header( "exploded", "width=475193431")
            .header( "nonEmpty", "height,179896299")
        .when()
            .request( "GET", "/header/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,496698538")
            .header( "exploded", "height=731104163,eczytztxyptdi=dT,j=-581.5")
            .header( "nonEmpty", "width,0,auruohhqhierkfxm,583,nimst,y}),qlzimcceauqhnt,udhbiupriny,,`4")
        .when()
            .request( "GET", "/header/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "height,232189845,emgzvgl,-510.2,hqawb,644.8")
            .header( "exploded", "width=0")
            .header( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/header/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=0,cwbs=M,fpptiwgbjaigper=2I)CGC\\,nybojgjp=343")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=0,qih=true,hwfupxiuhowxdc=true,gqjvmww=-452")
            .header( "nonEmpty", "")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=0,dmtbphyi=92,za=C,AP)h,gqqsvzmxmlbkoyvz=true")
            .header( "nonEmpty", "804.2")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nonEmpty.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=0,kmpdsrxlr=true,jur=true")
            .header( "nonEmpty", "width,-448.4,riwzbbreuq,408.2,cctzciwac,-851,hlbcqlmvbn,sizir,,qzkrvirbfis,iN?]-{,nchcz,625.5")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nonEmpty.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=0,rhpkum=-1010,ghyanqwdb=;")
            .header( "nonEmpty", "width,-1,ilofpcj,true,khqtwitt,Q.+kXK,jbduvn,")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nonEmpty.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=0,whsyvdwxeqte=_g[x:,0N,Yp{0,rfyjg=.C!7")
            .header( "nonEmpty", "width,296879204,height,true,jkayyivqlxyl,true,qqaiellerzxmcnrg,-216,xoigxgzocwq,L8'<~,,CN)?")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nonEmpty.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=0,cookzumruggwe=true")
            .header( "nonEmpty", "width,152171102,height,-1,bbipyajhmxndi,jaxg,>\"lfP,okzvetzrhh,,bppuzoatwev,624,wcqqh,true,jyuimtncbraltgkr,2C@x+")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nonEmpty.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "exploded", "height=0,lflwwlxrlm=4;_^dmW,x4IBj7")
            .header( "nonEmpty", "width,757824689,irwgzpmpa,gijmmtdhnj,true,rieymtgvatz,^,rdngjhjpklfu,-1016,rjlhibbmv,htioix,U,iuslsalrqhmvytox,,jlwtlyolrnojz,Q81")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NullableType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "-290")
            .header( "exploded", "height=0,gxhxzxyyzfltfx=353,oz=y;Zs0nAO")
            .header( "nonEmpty", "width,323520931,jxetgkodvu,r uU^,:^!jw[].,,Saq,rdeuxryasbclcw,-770,izitnkjb,^pe6D'p;")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nullable.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,true")
            .header( "exploded", "height=0,exr=of,234,yvqxffmmwqyojr=203")
            .header( "nonEmpty", "width,799718240,gugbvkmjhbeliuqg,278.8")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nullable.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,-1")
            .header( "exploded", "height=0,uyzrme=true,grmk=363.2")
            .header( "nonEmpty", "width,96597159,ftejwiitfq,true")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nullable.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0,height,439.0")
            .header( "exploded", "height=0,qalwaclkf=~,.{6r%_,6*b(rP$")
            .header( "nonEmpty", "width,202133079,gczycuox,-782.2,ttiobfaqsdwcd,true,rbfsznkyuajq,true")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nullable.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0,height,-1")
            .header( "exploded", "height=0,vzcaytupxrzag=,ahxmuynlfgasek=-280,tlymv=-307.0")
            .header( "nonEmpty", "width,829710387,cmurcrur,-964,chgwjriku,B,kc^w^eDr,yybdjk,y@0L\"{&G")
        .when()
            .request( "GET", "/header/object")
        .then()
            // nullable.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_ExplodedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "nonEmpty", "width,572550879,t,true,mhzqvpnwsxvtlzvm,wjbmaggtbu,-398.2,pnuiqvx,-380")
        .when()
            .request( "GET", "/header/object")
        .then()
            // exploded.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "")
            .header( "nonEmpty", "width,380466255,vnxmqfhnlyayyqq,255.7")
        .when()
            .request( "GET", "/header/object")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "pFjm-%yi")
            .header( "nonEmpty", "width,409561140,dteqkv,-694,nrldmcbhd,702")
        .when()
            .request( "GET", "/header/object")
        .then()
            // exploded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "width=irgwnhopt,true,igeiworenlgvuv,,E,_[,isyjajrzdfodckj,-497.2,height=0,wn=true,iaoiqpkcx=,umkezmazf=M$,4i@K")
            .header( "nonEmpty", "width,603601543,ouzlzq,mP,iajaiwy,895.3,fpdrbjuj,heojceiokgctfo,821.5,ikmtdfncefgrgv,A3jH`1,~9K,.)OQk")
        .when()
            .request( "GET", "/header/object")
        .then()
            // exploded.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "width=-1,height=0,p=rnaosldnk,yN\\p#,cikjfjmlnzqzh,-611")
            .header( "nonEmpty", "width,885503262,dbgsm,$+W,qrqtb,true,nzcwlmqcf,WM].$5,C2SjBW=")
        .when()
            .request( "GET", "/header/object")
        .then()
            // exploded.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=PyQF;9J],zghhbfxh=]P8\\,syqdcufgalks=-706,qknke=true")
            .header( "nonEmpty", "width,597257842,rxfnebpkyu,true,bf,true")
        .when()
            .request( "GET", "/header/object")
        .then()
            // exploded.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "width,0")
            .header( "exploded", "height=-1,otsplcb=")
            .header( "nonEmpty", "width,468372375,lnivpwodyiqsktu,true,rvtwhqqbye,true,rqtrnzgifbbp,gbZ|,(8e)")
        .when()
            .request( "GET", "/header/object")
        .then()
            // exploded.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "14cJYO&=(T>{hfh'##I/*uigZ6GmM&Pm%` %DKf!jA,:\\(DkCa \"+#%nm,B5L3@': xQ`YeX<nq9zrZ\\>W/i'FFp3[cDuks&|t5vCKQjPAF_@KB&HOD7y]8s#C'wGb{A1k?w(RX{T1Jrw#^pfc'")
            .header( "nonEmpty", "V[n")
            .header( "nullableNonEmpty", "")
            .header( "empty", "y")
        .when()
            .request( "GET", "/header/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderString_EmptyValueLength_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "nonEmpty", "0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwXgIZjb%WWDG:>2AvL#|L1:8E;u#>Dmy:o27HkDZ%rt)listU>?]sAviyah`46\"yhpb\"~'tm1ZV>& S{DOg6(3Wt")
            .header( "nullableNonEmpty", "")
            .header( "empty", ">n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg],I=BTHC6;vgsi}{1YsSiTi|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_o#)?iw1sv09rscxxp8%X/#>V|=KN(ip@[:+&rEEMcN.PyrD\\.9M5:</v2\\t|3I;&@@K~$JU@ihst_E[pP)^oQ)x4U1!` `FGJ+1Z~3\"P(Ys|!DI,F) >*$t{egzYY<JB](%E4N9BRTWHzX(0o|HhJW4")
        .when()
            .request( "GET", "/header/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderString_NullableValueLength_Is_0() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "nonEmpty", "M.w")
            .header( "nullableNonEmpty", ":\"QB6?Nu^7M%pKSwr/BFl6m7V$v2vn mLDewSA>qQZ=`bnm^hT=[M~LM]2iAtQ*{M*H< yoM\\Z<]t%QO{7_`&pEdwsXr3Fgmd J| =o_o")
            .header( "empty", "c")
        .when()
            .request( "GET", "/header/string")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "[`Pal#.G`.4{wR")
            .header( "nonEmpty", "Ebl'H<q%|EdyDlPp`z<rpJWEwMgaqlhIs6/-b9X9sg~x>>tB%,/k~ c\"V_Ak1T=Jco~ D\\X2>_qzE`@c% Kg,)xak\\Q:bcGrj6(F(g8\\'gX'uf8ui~Y+F>-X(m")
            .header( "nullableNonEmpty", "")
        .when()
            .request( "GET", "/header/string")
        .then()
            // empty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderString_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "(f3NDGZW$sU\\l,8dK4Ush*wg5cupL@=wy5R:7{*")
            .header( "nonEmpty", "}O$/dmfFfe^>quwQw]Cb_@R\"-+\"mX(o!+EUV<<>`=CBg2d\"Ow:e6KT6;IUFdOY}vLS&K8^S:F+,*V$`$p.VlD}[!xuGEGh_MV$i:26U'~lb<#Ctwr~#\\d8l= SP^A6>|V@<p idGE`zGK-[#'o/K*#@mPxlw4ejJJBF")
            .header( "nullableNonEmpty", "")
            .header( "empty", "")
        .when()
            .request( "GET", "/header/string")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderString_NonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "q\":CoWP^zw`E)Ss?gt@T+8''!Q7[>W$}xlw5C6xw}n975cB9C7d<1v[4&13(7giO~HtA7GA@;UM&<y/c^PS{u{j>X+c+mz=(BJ`)Q%Y?C~r2Y$Q^:~]7kY~)2XReg230od3D2R%rXN$wvKQOG\"UBx{D|=G#U_+$fF/-~Wv^Ij3QRe]QgSxqqIFk\"Y[cn<w:EYv3qSUy>{V@DVZ#y?]>?H^YNRWo,")
            .header( "nullableNonEmpty", "")
            .header( "empty", "67l'D{8AvIQT;wrW_2bSG0gNzVEBzAVheOMT3RzN`fTSro.#'Ql*w'|x)7 i+Fs/fIf)U^d)oG\"y0ls;+4F0T?1&^fD}V]j-U~2g=dDBY2MZ{Kjd$>-T")
        .when()
            .request( "GET", "/header/string")
        .then()
            // nonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderString_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "V2Zpid~eSCkct=I%B2H_}OtD^*b}mMlhvG/h+kgJ1|Rx^X<fl0>UO! -:}'OT7c(VNF8W6H@5HYFGoE`c@T\\Gq7oQ^LQ_kr{l~f-$mJSHA\\.TsZ#y,Rd>5V+#^f(Fn2!IV^~0]\\K#|k%mTt5y_bI&q:q1a ]DkTwZx7-AD3m,>56P~)=\\&QQsh0pC^0iZ][l9hgky;oYMI!>2AoP<x&V'tL2DG:tZX`sd3(d#D_jIn=h;hylj]hjD7")
            .header( "nonEmpty", "")
            .header( "nullableNonEmpty", "")
            .header( "empty", "aoIG0PxZFw4IEWD78](;}-T~CjbL+qt:p_74B*rt)(Tl`faB*GO|3>~N:L.WROM U[)6t|]a]_`hvb;.k(ov:4,\"/w)v]B`VV /n\"LB0MuH\"Vt]W`{J{;Z73tB}7LL2Z}+iO3pFe\"^6&7XQZm85j,n,O<w kD)Awt0K93L=D")
        .when()
            .request( "GET", "/header/string")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderString_NonEmptyValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "U~WkmaNOR+^*I!`zy}pD<,A'3@3h\"|uXQNd8yCU)ow`xSOFxIfrc] 7vY.YGU|Kc'2e)_:uH$clo$xDn`nk$U$&b\"JPJ5YeDxMR%(]necb|EUa[1zWu\\2y$,D_@7&swhL9~o(XbW-U=gv2|**b6;A`zNrty}j!qHxVXZy\\T-qIU=?KXIcQ*)(LRO=;Prs$(7")
            .header( "nonEmpty", "hM")
            .header( "nullableNonEmpty", "")
            .header( "empty", "*Y kq>ueSUy ,wr2B*~x_x!ZFAb@QO>sj=IV'r?#C|*\\nk\"CS$4]BVTuU@R0Gq8xc}'mJq4o7D;WVKqu*6CV&=,8mrKV{")
        .when()
            .request( "GET", "/header/string")
        .then()
            // nonEmpty.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderString_NullableDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nonEmpty", "c+.xCN%\"_}s FcMKg9ok`=Plt6J~_fOgLwQ9IPH4v9}9|G!?Sdkj6i'o$1JrJ96'pyFgS=2'4!1H~Qi.X)p|`2#XkY[5{#_@&\\$<N]K=Q'F4#R*-j\\m\\j|j/3!ZfYWo{qEFoZ)DiMa(K/\\DW;u*j\"C?lsNptCBOuUr%$^|m=]z I3SY]ud.c3~SAY'D[yHHC'd)5x|Q4M*|:s;2C]6*W`hSOgR!WOv.`N")
            .header( "nullableNonEmpty", "")
            .header( "empty", "W~bmLQt;xE\"vuk *Tgu%?0VVt+\\{/hLP0GY/@xG=LFzsm;bVCvW/tcT8")
        .when()
            .request( "GET", "/header/string")
        .then()
            // nullable.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderString_NullableNonEmptyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "FS/Z* ?o4wol8TB#J>UY]%+~vtHp$FXd`B0g4X|\\{K\\E/&sLj*!I7urAs13./#r#1,io.:N=YWu;b?(Z_cbh.G,MuF0XeE ~#!|/Ak<[~Jf W)HF9%I) `HR$pYUUgI*mIf`Ijesidzsv~'Y$rz3[q|BV\"N@~\\/d:}%rbwRqnyV]jkUpTTd'QGW!q TmR@T*9a^NIzCWE)6,yhEX9,l")
            .header( "nonEmpty", "k6f])m{qle^74zeSR9{aNY4G 2#jtKv37$\\jT,L6KVsJ3YCiZ;57XZP{svzzo`Dt[,:5M")
            .header( "empty", "**Hr#daxwST;zEvc&i`ez/#)nj/`bu_1=#te}sG^IGe3_[{zNsZ8Xd|c\"i6A-Wx!Rcb;RA-uYS6`u^_3h8q:pK&psPFAIT^CK&Y3{!m1rS.?UlY$Iz|\\t<1'\\dI~;~TL[,l&o<=@2[g't;~ZxbRh1[e9({jKgZpgVB=`7^QG\"8YgUlbB%_-^XuZ!z@+:YY'BEZiq\\2=UYMof13'XuWaQn{s$:f% '2[M{Y}@m`Ox")
        .when()
            .request( "GET", "/header/string")
        .then()
            // nullableNonEmpty.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-915629898")
            .pathParam( "exploded", "-777592863")
            .pathParam( "nonEmpty", "-787864396,968195964,-941125629")
            .pathParam( "empty", "-479634177")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsSize_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", ",-290067183,-159033827,-879373065,-998839893,1007387311,840979781,718184765,-719901843,212473142,-466899496,608407672,-599674523")
            .pathParam( "exploded", ",-660351823,-362664233")
            .pathParam( "nonEmpty", ",226034055,-884703973,226034055")
            .pathParam( "empty", ",-97868370,-279296209,485709141,-216666114,-453603517,-935803584,963242121")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_0() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "0")
            .pathParam( "exploded", "0")
            .pathParam( "nonEmpty", "0,-790520378,464352219")
            .pathParam( "empty", "0")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "247662958,206516015,206516015")
            .pathParam( "exploded", "339122953,339122953,-164249220")
            .pathParam( "nonEmpty", "597556810,-739813784,-10240067,47029742,757335484,822581821,-956279549,220628827,-858529578,895300504,-739813784,722180891,170367393,683319538,703520610")
            .pathParam( "empty", "117826646,117826646")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-1044207262")
            .pathParam( "exploded", "-210497736")
            .pathParam( "nonEmpty", "-779353904,665084706,396090616")
            .pathParam( "empty", "")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-581166028")
            .pathParam( "exploded", "-613571985")
            .pathParam( "nonEmpty", "-551185881,168692740,-987473178")
            .pathParam( "empty", "true")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // empty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-777374064")
            .pathParam( "exploded", "-365727516")
            .pathParam( "nonEmpty", "-527133846,772732844,-328103540")
            .pathParam( "empty", "-213.4")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // empty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-213474350")
            .pathParam( "exploded", "-480690895")
            .pathParam( "nonEmpty", "")
            .pathParam( "empty", "-449024820")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-194716814")
            .pathParam( "exploded", "-978628238")
            .pathParam( "nonEmpty", "-545.9")
            .pathParam( "empty", "-448965344")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsSize_Is_2() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-513574421")
            .pathParam( "exploded", "-272279337")
            .pathParam( "nonEmpty", "-752797874,-752797874")
            .pathParam( "empty", "-621459142")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Items.Size=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-728126616")
            .pathParam( "exploded", "-321574503")
            .pathParam( "nonEmpty", "hnd,909.0,edrwghygiso,764.4,485495228,998536309")
            .pathParam( "empty", "-791141610")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "")
            .pathParam( "exploded", "-41580670")
            .pathParam( "nonEmpty", "-732336218,109949038,-1044123185")
            .pathParam( "empty", "-1027669540")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "395.2")
            .pathParam( "exploded", "-57818659")
            .pathParam( "nonEmpty", "-141106705,1061086333,-945685656")
            .pathParam( "empty", "-796367007")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "ximwsaxjsd,5as,dlzzomzfuknzqlju,315.3,gmbpaka,-970")
            .pathParam( "exploded", "-221863041")
            .pathParam( "nonEmpty", "-461472705,-748673657,551942108")
            .pathParam( "empty", "-887294233")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-867795673")
            .pathParam( "exploded", "")
            .pathParam( "nonEmpty", "-987923883,-267637067,-164428690")
            .pathParam( "empty", "-823710164")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-882107883")
            .pathParam( "exploded", "(#ur5")
            .pathParam( "nonEmpty", "-223882716,894636233,-781422867")
            .pathParam( "empty", "-277231493")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "-423575356")
            .pathParam( "exploded", "T.n+,e/QX")
            .pathParam( "nonEmpty", "-426301194,655510060,-188352479")
            .pathParam( "empty", "-365942331")
        .when()
            .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "")
            .pathParam( "exploded", "")
            .pathParam( "nonEmpty", "")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertyCount_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0,height,0,cmpwhef,-200")
            .pathParam( "exploded", "width=0,height=0,qrvgh=true")
            .pathParam( "nonEmpty", "width,0,height,0,imukwskokuxrpx,true,dexxjvohnhiu,Y>.Sk")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,")
            .pathParam( "exploded", "width=")
            .pathParam( "nonEmpty", "width,")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "height,,idy,")
            .pathParam( "exploded", "height=,adqbzdj=")
            .pathParam( "nonEmpty", "width,480945323,height,,dsyxirbchglys,79,ww,639")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,825195508")
            .pathParam( "exploded", "width=573092271")
            .pathParam( "nonEmpty", "height,1019957726")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "height,3725862,ocslwsgotyzi,-689,aqfmjf,0'")
            .pathParam( "exploded", "height=724595442,tcbgoxaa=629.9")
            .pathParam( "nonEmpty", "width,0,yxrqcwtokegnltvf,")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "true")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "width,tljqlwttimhkxjjh,-217.0,fmpapzhxplitxbnf,486,xpppioawrm,qI^e,height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "width,-1,height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,821.8")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,-1")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nonEmpty.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "48.5")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,W}8,w RJ=\"8")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,-1")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0,height,nwnhrxnji,l!&( 3hQ")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0,height,-1")
            .pathParam( "exploded", "width=0")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // nullable.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "3P{")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=`")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Value.Properties.width.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=-1")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Value.Properties.width.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0,height=519.9")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Value.Properties.height.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "width,0")
            .pathParam( "exploded", "width=0,height=-1")
            .pathParam( "nonEmpty", "height,0")
        .when()
            .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
        .then()
            // exploded.Value.Properties.height.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "A")
            .pathParam( "nonEmpty", "S0)")
            .pathParam( "empty", "$")
        .when()
            .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyValueLength_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "hhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wzx6{<h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=")
            .pathParam( "nonEmpty", "\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*aG)@b&1avN")
            .pathParam( "empty", "IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^")
        .when()
            .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "}")
            .pathParam( "nonEmpty", "o%z")
            .pathParam( "empty", "")
        .when()
            .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
        .then()
            // empty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "L")
            .pathParam( "nonEmpty", "")
            .pathParam( "empty", ".")
        .when()
            .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
        .then()
            // nonEmpty.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyValueLength_Is_2() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "P")
            .pathParam( "nonEmpty", "f%")
            .pathParam( "empty", "z")
        .when()
            .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
        .then()
            // nonEmpty.Value.Length=2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NullableType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "nullable", "")
            .pathParam( "nonEmpty", "K{2")
            .pathParam( "empty", "h")
        .when()
            .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
        .then()
            // nullable.Type=null
            .statusCode( isBadRequest())
            ;
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
