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
            .header( "exploded", "-211869667")
            .header( "nonEmpty", "-821215524,-119190888,586414811")
            .header( "empty", "-203695942")
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
            .header( "exploded", ",632149186,517667921,270644871")
            .header( "nonEmpty", ",,715999599,-660477096,316025043")
            .header( "empty", ",206057907,-201030541,601722898,776217297,653666552,687714008")
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
            .header( "nullable", "-508382795")
            .header( "exploded", "0")
            .header( "nonEmpty", "0,-825297674,688173451")
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
            .header( "nullable", ",-642512587,-459216688,-685996952,890866742,-344193146")
            .header( "exploded", "953813919,384443308,1066371096,-926981016,-926981016,-267135836")
            .header( "nonEmpty", "742461240,188088590,769523032,-721399090,-872382691,-872382691,-315604707,-957012514")
            .header( "empty", "333798856,333798856")
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
            .header( "exploded", "-888040185")
            .header( "nonEmpty", "-773730152,1000242741,47038037")
            .header( "empty", "-513857057")
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
            .header( "nullable", "654175967,-474099474,32535507,-211318121,-864468819,1072911013,-1002281292,281603777,-15574643,-914771050,-602321262,-864468819")
            .header( "exploded", "0,-456085276,748508990,-741370177,1028913427,853241509,-619510627,-52055178,-899016760,821071033,337300929,206578279,237626193,149560142,-1024936951")
            .header( "nonEmpty", "0,-415015221,-996221755,969259360,969259360,-64375633")
            .header( "empty", "0,-422326822,154775506,737735690,-791789172")
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
            .header( "nullable", "")
            .header( "exploded", "810225467")
            .header( "nonEmpty", "590219265,-858388135,-408317494")
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
            .header( "nullable", "")
            .header( "exploded", "482347027")
            .header( "nonEmpty", "863784747,-643378508,743289889")
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
            .header( "nullable", "")
            .header( "exploded", "5208811")
            .header( "nonEmpty", "904575538,455091147,-561249414")
            .header( "empty", "21")
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
            .header( "nullable", "")
            .header( "exploded", "748353541")
            .header( "nonEmpty", "365203033,313128538,-259778064")
            .header( "empty", "n:{%!")
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
            .header( "nullable", "")
            .header( "exploded", "739058877")
            .header( "empty", "165796036")
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
            .header( "nullable", "")
            .header( "exploded", "792206709")
            .header( "nonEmpty", "")
            .header( "empty", "720017796")
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
            .header( "nullable", "")
            .header( "exploded", "1065087361")
            .header( "nonEmpty", "-740.3")
            .header( "empty", "1067679508")
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
            .header( "nullable", "")
            .header( "exploded", "869768444")
            .header( "nonEmpty", "689828071,689828071")
            .header( "empty", "300127986")
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
            .header( "nullable", "")
            .header( "exploded", "739970185")
            .header( "nonEmpty", "767.2,699812567,-78655875")
            .header( "empty", "118428949")
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
            .header( "exploded", "765361920")
            .header( "nonEmpty", "285620401,-4174088,-388066517")
            .header( "empty", "102568843")
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
            .header( "nullable", "true")
            .header( "exploded", "610382830")
            .header( "nonEmpty", "103499784,309728522,-1052101273")
            .header( "empty", "910465997")
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
            .header( "nullable", "/C|6h:")
            .header( "exploded", "235331091")
            .header( "nonEmpty", "785075086,962602474,-861872159")
            .header( "empty", "1001200157")
        .when()
            .request( "GET", "/header/array")
        .then()
            // nullable.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getHeaderArray_ExplodedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "nullable", "")
            .header( "nonEmpty", "707791177,72119463,735625234")
            .header( "empty", "595014723")
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
            .header( "nullable", "")
            .header( "exploded", "")
            .header( "nonEmpty", "744186034,-676957197,-620857914")
            .header( "empty", "648192509")
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
            .header( "nullable", "")
            .header( "exploded", "e3R^L")
            .header( "nonEmpty", "1064995581,113856763,882740148")
            .header( "empty", "7891770")
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
            .header( "nullable", "")
            .header( "exploded", ",\"r)FS")
            .header( "nonEmpty", "725465616,1029074970,-286890728")
            .header( "empty", "863964201")
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
            .header( "exploded", "width=0,height=0,eh=TB#J>U,]%+,vtHp$F,lweyndokmmixg=-926")
            .header( "nonEmpty", "width,0,height,0,vlrxfgfja,true,lz,637,jaltgzpdy,true")
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
            .header( "nullable", "width,0,height,0,bvheeftpx,true,lnyjmz,~#!|/A,pfemnjnhznn,) `")
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
            .header( "exploded", "height=,jygbrlgrtn=,qdblvayphy=rz3[q|B")
            .header( "nonEmpty", "width,425163625,height,,nroxm,fqfzhwbnfjnid,-261,ihhfzx,R@T,9a^NIzCW,tcqerwggkk,f])m{,,ezbpybiwoetlbnpf,gmnhatdtqnuxihci,CiZ;57X,rljwuqgrp,,:5")
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
            .header( "nullable", "height,,vzz,,pjuzbdnzhn,ez/#)nj,yhtsxlhpxlkoi,true")
            .header( "exploded", "width=454826932")
            .header( "nonEmpty", "height,552898407")
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
            .header( "nullable", "width,239043247")
            .header( "exploded", "height=822714219,gykrtzpu=-335.9")
            .header( "nonEmpty", "width,0,mzxsxvcoxjvs,sbxnuabklmgmzd,166.1,tfvfqmgu,514")
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
            .header( "nullable", "height,490022368,cclhvnrzighsjzv,")
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
            .header( "exploded", "height=0,pekalpfph=2[g")
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
            .header( "exploded", "height=0,kmdfbf=,9({jKgZp,V")
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
            .header( "exploded", "height=0,jkusk=-140,sblb=-^,")
            .header( "nonEmpty", "z@+:YY'")
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
            .header( "exploded", "height=0,ocoxr=-1001,ilbtweyaxvstucy=% '2[M,h=-1002.4")
            .header( "nonEmpty", "width,xe+!vi,jdwlywhsffxao,,pyewpuri,-248.4,uljekfhtbgdvo,")
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
            .header( "exploded", "height=0,bfddatckbign=-60.9,ekvqxf=kkwtbepbp,,xyotqrwtwxn,-916,gvdqnwgr=zriwn,-847.0,xbojirukpcbi,true,wcbburixuznl,309.8")
            .header( "nonEmpty", "width,-1,uwpmiokjjmqfnjww,qwoeptmghzaepr,-725,nv,m)%{1LG,A,s $@87,gl,,(,xsucsxja,true")
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
            .header( "exploded", "height=0,qxpvtosqskqwg=DWL;$")
            .header( "nonEmpty", "width,971035747,height,icjg,-674.5,b,true,bujzij,294.2,dj,-65")
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
            .header( "exploded", "height=0,o=-570,ijfhhat=551.3,gveyrfwfyeqi=14")
            .header( "nonEmpty", "width,777912810,height,-1,gleofcbzymgnb,498,w,-41.8,osyjc,981.5")
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
            .header( "exploded", "height=0,y=-59.9,ynbv=fdjufrgnpy,6*@fu>u,vmuyutzxxlrj=-0.3")
            .header( "nonEmpty", "width,149430003,fmikjohxwfyrs,true,f,164")
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
            .header( "nullable", "919.0")
            .header( "exploded", "height=0,xzodh=-231")
            .header( "nonEmpty", "width,561447674,csgetiojm,\"EJ|,,khcfz,-890.4")
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
            .header( "exploded", "height=0,upfwl=true,nfknvtpfxnhh=true,uxyqhoxrz=")
            .header( "nonEmpty", "width,463064133,xy,-127.5,qcmcyibbjfsg,;JI=jmuT")
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
            .header( "exploded", "height=0,rfxitohmaz=-873,v=]`OZG,x1i,wzhlsqk=-658.3")
            .header( "nonEmpty", "width,211677099,euqgas,o{FN;f,E95C$,C,iiwezfqvwob,true,grzdo,71")
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
            .header( "nullable", "width,0,height,Z")
            .header( "exploded", "height=0,kgdbdjnbsqkjuw=66")
            .header( "nonEmpty", "width,368321762,yncabhgwtefte,2^</,;!1rXi,sgjcdlennrh,460")
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
            .header( "exploded", "height=0,ofglpwolohen=true")
            .header( "nonEmpty", "width,233983455,wnvojyjsuzv,")
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
            .header( "nonEmpty", "width,269627969,xwiwhqfbj,2?WSw8t")
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
            .header( "nonEmpty", "width,374464310,dblvrcimdz,468.9,ctmargmbykui,$,vltnrkjyzxhie,ttnhizfyhbqz,?!3{,mbtc,,CE|e,2BsD8K,sjur,364.6")
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
            .header( "exploded", "true")
            .header( "nonEmpty", "width,427125644,kjbhztczkygjjj,-768")
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
            .header( "exploded", "width=true,height=0,itkiblomjgx=},n=Om\"D-Xd,kz}y+1mf,gz=-159.2")
            .header( "nonEmpty", "width,607656504,nvvpeqmisd,,Xu,zojduuthppakwkxf,124.4")
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
            .header( "exploded", "\"width=-1,height=0,plup=jlwryyhqnzfzjvk,true,diaspjujihlpr,true,vkuusefizgzi=Chqkd>?,_2_,HS \"")
            .header( "nonEmpty", "width,939057340,muqktmzephlr,;<u{")
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
            .header( "exploded", "height=445.1,iqqf=,pkesgvdw=i|=##m,,QqX!.*zC")
            .header( "nonEmpty", "width,770287592,tbawgnkdopqfvkxh,c,<,txyptdirjj,850,pisudauruo,13.0")
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
            .header( "exploded", "height=-1,erkfxmeignimstp=-66.2,d=")
            .header( "nonEmpty", "width,99895380,cceauqhntk,true,udhbiupriny,408.2,apqafemgzvgl,-510.2")
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
            .header( "empty", "(")
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
            .header( "nullable", "\"QB6?Nu^7M%pKSwr/BFl6m7V$v2vn mLDewSA>qQZ=`bnm^hT=[M~LM]2iAtQ*{M*H< yoM\\Z<]t%QO{7_`&pEdwsXr3Fgmd J| =o_oc=[`Pal#.G`.4{wREEbl'H<q%|EdyDlPp`z<rpJWEwMgaqlhIs6/-b9X9sg~x>>tB%,/k~ c\"V_Ak1T=Jco~ D\\X")
            .header( "nonEmpty", ">_qzE`@c% Kg,)xak\\Q:bcGrj6(F(g8\\'gX'uf8ui~Y+F>-X(mg(f3NDGZW$sU\\l,8dK4Ush*wg5cupL@=wy5R:7{*B}O$/dmfFfe^>quwQw]Cb_@R\"-+\"mX(o!+EUV<<>`=CBg2d\"Ow:e6KT6;IUFdOY}vLS&K8^S:F+,*V$`$p.VlD}[")
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
            .header( "nullable", "xuGEGh_MV$i:26U'~lb<#Ctwr~#\\d8l= SP^A6>|V@<p idGE`zGK-[#'o/K*#@mPxlw4ejJJBF<m?msiX,SRO\\FRR`~6E'JU(8+l\"o! G-M3VX_UxHz~rHo&3mshX&U{Bu9J_GI'ekwfY})Fo{JuFa}]]Sd8$&x9_%Bh,Bu3p}9%{Q,2e>=R/-)D`0Q;@f_~3+2Dys=ohR+{T8*hW$:%2Z7Udz)V")
            .header( "nonEmpty", "Kq)8$Zc\\DOqfvFSDLh@^8BwBC)q*6(H](E4iEMT205M.w2pMI;$BwM\"4-/cp^f1W<R,',~~6E'{_>(ZTh_wt'k_.uo}b#<`j^P@xz&}?\"e,XS$plN ~[0E(:bVT}nN+")
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
            .header( "nullable", "0V_Pd`-3xbZmws?v2Rpj/f1^i^2iVySVmj8~t++\".1L9Mk_d~2x&q}7Lf19C-A Kg~K4XfU^7i&{<p94(=j4*lDd@=q| o?%]s rPt/*vom*\\V:v/&5fr=P=Wa~s''#q\":CoWP^zw`E)Ss?gt@T+8''!Q7")
            .header( "empty", ">W$}xlw5C6xw}n975cB9C7d<1v[4&13(7giO~HtA7GA@;UM&<y/c^PS{u{j>X+c+mz=(BJ`)Q%Y?C~r2Y$Q^:~]7kY~)2XReg230od3D2R%rXN$wvKQOG\"UBx{D|=G")
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
            .header( "nullable", "U_+$fF/-~Wv^Ij3QRe]QgSxqqIFk\"Y[cn<w:EYv3qSUy>{V@DVZ#y?]>?H^YNRWo,V67l'D{8AvIQT;wrW_2bSG0gNzVEBzAVheOMT3RzN`fTSro.#'Ql*w'|x)7 i+Fs/fIf)U^d)oG\"y0ls;+4F0T?1&^fD}V]j-U~2g=dDBY2MZ{Kjd$>-T,V2Zpid~eSCkct=I%B2H_}")
            .header( "nonEmpty", "")
            .header( "empty", "tD^*b}mMlhvG/h+kgJ1|Rx^X<fl0>UO! -:}'OT7c(VNF8W6H@5HYFGoE`c@T\\Gq7oQ^LQ_kr{l~f-$mJSHA\\.TsZ#y,Rd>5V+#^f(Fn2!IV^~0]\\K")
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
            .header( "nullable", "t:p_74B*rt)(Tl`faB*GO|3>~N:L.WROM U[)6t|]a]_`hvb;.k(ov:4,\"/w)v]B`VV /n\"LB0MuH\"Vt]W`{J{;Z73tB}7LL2Z}+i")
            .header( "nonEmpty", "O3")
            .header( "empty", "Fe\"^6&7XQZm85j,n,O<w kD)Awt0K93L=D4r>!=d`mRox8mx@e$0`6H~iAJ{,XtT=@)4W8r<_*]WTp_9;b?2:piu4RDl`UPTIE$A9=KuGLSrg}M.fErV+$@wZ$v7%D\\z5soZ<Hi|o^EIU!<rs+jt<}sEKs0:]x`Qv$S(P\\Q~Qkr]6L>}DXTz k$O,pt.z<#82?;$U")
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
            .header( "nonEmpty", "wF$ VpxtPj\"aye\"\\g:hAWO#!{HloWxb$_`[x'0,m}qV$6|@WwWnU;KcSda5m\\l NDt7<jmHGOIwc*Eb+3;L=Axq")
            .header( "empty", "`&Q?D%tIK]JG+/:?U/WIyoq\\.U`o'gMllr+uCfq[O5+\\~c!%U~WkmaNOR+^*I!`zy}pD<,A'3@3h\"|uXQNd8yCU)ow`xSOFxIfrc] 7vY.YGU|Kc'2e)_:uH$clo$xDn`nk$U$&b\"JPJ5YeDxMR%(]necb|EUa[1zWu\\2y$,D_@7&swhL9~o(XbW-U=gv2|**b6;A`zNrty}j!qHxVXZy\\T-qIU=?KXIcQ*)(LRO")
        .when()
            .request( "GET", "/header/string")
        .then()
            // nullable.Defined=No
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
