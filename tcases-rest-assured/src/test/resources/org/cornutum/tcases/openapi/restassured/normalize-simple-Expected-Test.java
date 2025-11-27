package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class NormalizeSimpleTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getHeaderArray_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-776646755")
                .header( "nullable", "")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "-365797161,691972744,673698912")
                .header( "empty", "-380184617")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", ",-255862890,351939176,894198957,-93501521,104806809,360398643,1045124066,331039137,230722781,-499109668")
                .header( "nullable", "")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", ",-932814604,-552260893,-84440946,-552260893,309480690,705418352")
                .header( "empty", ",-23422220,972091554,631353247,619062888,-567078293,734230245,-185751222,-1025190257,779210755")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "-512302342")
                .header( "nullableNonEmpty", "-896438212,-270332143,-234906661,462423158,-325409180,-850724430,-917808615,464385673,890085130,617146312,290427985,-528826170,87876097,-1065004142")
                .header( "nonEmpty", "0,668270781,-548880879")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "142360039,422075878,-506828351,422075878,471292943")
                .header( "nullable", ",870779198,-1019780051,562153208,740312557,-142699090")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "120754233,-178192870,-521654952,-178192870,-452095163,67119055,-135385010,1034883342,-584141731,-414145721,1060861624")
                .header( "empty", "296852073,989857126,-422761640,989857126,384565277,-642612912")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-329778607")
                .header( "nullable", "0")
                .header( "nullableNonEmpty", ",760985355,760985355,674627519,-1066673733,936151253,658591365,-757231755,-531883218,607864486,-409743021,-262341487,731348640,-750047856")
                .header( "nonEmpty", "-242423866,-147302448,-980202951")
                .header( "empty", "-962892928")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0,426059454,-182315734,298480677")
                .header( "nullable", "678286331,-389629053,-470764369,-389629053,596355893,759325089,-22619314,-358101464,592828057,-458952140,473080002,289278930,-492681258,-779286469,144753004")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,375860753,-512136768,908842701,772525553,-482914771,-174907580,-99211370,660104834,772525553,684561939,-756605235,-450526459")
                .header( "empty", "0,-631302482,-695268879,628276802,136845909,-363103248,695516826,979215884,-120601044,-1008973625,-572419815,-449929033")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "802875120")
                .header( "nullable", "")
                .header( "nullableNonEmpty", "0,-525219417")
                .header( "nonEmpty", "340607162,267850201,-838077850")
                .header( "empty", "44281131")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-977445778,-1059454539,-993266536,660130446,-102706077,-977445778,1041506275,-574802857")
                .header( "nullable", "-228540767")
                .header( "nullableNonEmpty", "953951781,562391096,562391096")
                .header( "nonEmpty", "-292700727,397221992,107348705,397221992,-88182886,-838684499,450810644,490125460,-123281351,20731913")
                .header( "empty", "-393311919,-393311919")
            .when()
                .request( "GET", "/header/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,595294443,61404077,308632873,-163982193,-91078445,-922186306,-501227642,846127226,-914579436,80156923,347140568,-373629324,463597989")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,767866155,500059247")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,294108266,-415596357,952161561,-991640072,-508296392,708290815,672651064,942985147,-887075963,-380633963,752444287,-297785017")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,160310445,-295829014")
                .header( "empty", "")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,544268945,336632291,-417811498")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,505346965,838146559")
                .header( "empty", "\"9o")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,-161990624,-1042620397,-124121987,43520868,-169782167,-737813284,-460654512,-12894697,863181495")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,197436499,569862538")
                .header( "empty", "true")
            .when()
                .request( "GET", "/header/array")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,619616696,-472648652,-253914298,-952261962")
                .header( "nullableNonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,856630782,-859469848,915663614,-668493335,-232261185,851217512,702387208,-115447478,-255608405")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,177451631,441341960")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "-27.3")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,104134348,-1044175865,773749342,616457118,-808731313,794122691,223523017,459695189,-823841463,961820929,-852229588,961564007,431077872")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,0")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,-857281403,613389621,267508627,-15002642,717677234,101020620,166455314,-934009414,-677667374,371970064,-856128000,-494017518")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "cfmikjohxwf,916,vqfqfhmcx,true,-318562414,280798610")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-603658663,-98320100")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "true")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,462182147,57243451")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "-968.0,-530402359,336499925,-772049311,-882970886,-1072776539,-422145851,-305459000,-425915437,981451665,50917333,-191279345,400803909,487510729,849073704")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,2753524,985736034")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,668558721,-889047644,-128303171,534366710,-217565594,-994611042,369146270,-222029535,818543088,667821749,-535501089,777657227,-249156046")
                .header( "nonEmpty", "0,890747956,137755239")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,-798265532,464061724,311862592,744638460,-823095573,235672712,-958632106,813032852,-471384858,-648257486,-958815794,1030595854,1015327192,-408266226")
                .header( "nullableNonEmpty", "xy,367.6,rzqjyukxyjkr,Q%l")
                .header( "nonEmpty", "0,838941181,-156732865")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_NullableNonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "0")
                .header( "nullable", "0,-604370114,-795162578,-464615612")
                .header( "nullableNonEmpty", "-520.9,768407574,405925304,-744863277")
                .header( "nonEmpty", "0,-869859539,-759412114")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // nullableNonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "0,-649774598,859805674,-78928759,251035947,435622284,263617608,853776598,559338153,1058144316,-695552803,-1069390196,2840475,-545797367")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,795738230,357276767")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "")
                .header( "nullable", "0,-976302452,678250908,925691103,20413021,27827168,-938233287,689286766,-228186011,495270445,283604821,-807657252,23741015,-736839981,-393081095")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-247254367,-802268599")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "41")
                .header( "nullable", "0,-24446909,-966322417,-719116997,-411900912,314307630,-169690451,-862064725,-549577798,-383501076,-214998167,877713131,-209548659,-599409023,-533419001")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,912829613,-193059441")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderArray_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-32.1")
                .header( "nullable", "0,838171297,-591057632,671787927,348512206,-553849820,1043270797,375210383,371026107,-158591579,-765458501,-481820307,189573554,-84404664")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "0,-795007481,-780342462")
                .header( "empty", "0")
            .when()
                .request( "GET", "/header/array")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=0,height=0,bdcgrzdomgupph=@CYExxBl,Ga},/|`P#~N,abhgw=zJ`h")
                .header( "nullable", "")
                .header( "nonEmpty", "width,0,height,0,k,/@;!1")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=")
                .header( "nullable", "")
                .header( "nonEmpty", "height,")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=,etsgjcd=true,nnrhyuybo=-682.0,wol=")
                .header( "nullable", "width,0,height,0,x,827")
                .header( "nonEmpty", "width,,nvojyjsuz,true,txitxwiwhqfbj,2?WSw8t")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=374464310")
                .header( "nullable", "width,")
                .header( "nonEmpty", "width,509383929,height,206029867")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=418936626,vrcimdz=468.9,ctmargmbykui=$")
                .header( "nullable", "width,592256150,height,,tnrkjyzxhieoah,-645.3")
                .header( "nonEmpty", "zfyhbqzsp,hgbmbt,#bI%CE,rcod,176.5")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=0")
                .header( "nullable", "height,972238689")
                .header( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/header/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,juraets=kjbhztczkygjjj,bY Gx,S]%2QH>,ikq,-479.2,ierwxvksaero,true,x=")
                .header( "nullable", "width,0,lyjkzonvvpeqm,")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,aysqhhzo=0\",hppakwkxfpo=-282.7,lupgkujlwryyh=,sqS4?y1")
                .header( "nullable", "width,0,spjuji,17.2,ouvkuuse,c,ziirvhkpxen,2_jH")
                .header( "nonEmpty", "")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,kkumuqkt=phlrvvwva,true,etpmiqqfjg,,[;b,,*")
                .header( "nullable", "width,0,ltw,8b,ozflzfavlctb,-552")
                .header( "nonEmpty", "-235")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,femgzvg=RjuU6&,bhbvticwb=820,qkfpptiwgbjaigpe=true")
                .header( "nullable", "width,0,jzbvg,906.2")
                .header( "nonEmpty", "width,,?a,],nutqihpn,,upxiuhowxd,-876")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,q=967.0")
                .header( "nullable", "width,0,uxundmtb,true,yicpazagrxjikl,,gqqsvzmxmlbkoyvz,true")
                .header( "nonEmpty", "width,-1,mwbkmpdsrx,371.4,jur,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,bpxriwzbbreu=,`'$")
                .header( "nullable", "width,0,ciwa,946,ihlbcqlm,a,nksizirc,t~Q-)%h,[-B!iN?")
                .header( "nonEmpty", "height,true,bnchczxgp,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,hpkumyiyghyan=-209,bhdjqlilofpc=U")
                .header( "nullable", "width,0,qtwittvdymvw,132,jbduvn,,mw,true")
                .header( "nonEmpty", "height,-1,vd,qtekm,51,ojhnwv,true,wbvrrfyjgtvili,-156.8,ljkayyivqlxylftq,,ellerzxmc,-655.0")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,zxoigxgzocwqcm=421,iiqwqkji=56,oco=")
                .header( "nonEmpty", "m,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,gwezrmvbb=693,ajhmxndign=-264,xgrxdgyqyveokzv=rhhhclbpp,-316")
                .header( "nullable", "true")
                .header( "nonEmpty", "wevffgwcqqhv,uimtncbraltgkr,true,piv,-701.4,lflwwlxrlm,-578.1")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,ezccpvwdueb=true,ueirwg=true,mpawepgi=49.5")
                .header( "nullable", "width,,jovrieymtgva,527.9,vordngjhjpklfuvm,true,jlh,mvqykhtioixqlax,(u|s7,rqhmvytoxttajlwt,true,olrn,-285.5")
                .header( "nonEmpty", "ooppkje,true,xhxzx,duZ4]}Py,bF0y;Z,0")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,nsqhj=true,tgkodvuuo=307")
                .header( "nullable", "width,-1,ykykhmsqhqjwuxlt,")
                .header( "nonEmpty", "uxryasbclcwia,o[OA,<,ziafkg,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,nt=true")
                .header( "nullable", "width,0,height,S,Uuv|,nyvqxffmmwqyojrw,,>,rjt3<Q")
                .header( "nonEmpty", "e,-96.0,gjrvtauyzrmevrgr,7g2D|],i/othx\\,Shw,,dqalwaclk,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=0,myojoedyajzyrp=eejgczycuox,*Bv',obfaqs,96")
                .header( "nullable", "width,0,height,-1,nmrbfsznkyua,LU,vzcaytupxrzag,")
                .header( "nonEmpty", "hxmuy,true,fgasekinwtlymv,-307.0")
            .when()
                .request( "GET", "/header/object")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "width,0,dc,641,crurawhchg,E?}|DJBF,c^w^eDrJ,ybdjkhg,267.6")
                .header( "nonEmpty", "lycqno,878.7,h,900.8")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "")
                .header( "nullable", "width,0,wsxvt,true,v,,wwjbmaggtb,")
                .header( "nonEmpty", "xpnuiqvxlhxklvn,-750.6,hnlyayyqqdcgxn,762")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "-52.8")
                .header( "nullable", "width,0,xznm,-515.5,qkvifenrl,pQcM`hs,mirgwnhoptllig,")
                .header( "nonEmpty", "renlgvuvwpjin,[Q=dKXW,rzdfodckjkuv,810")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=foeqbfikmtdfncef,true,height=0,vcuwurzeishkgqj=true,ekb=650.0")
                .header( "nullable", "width,0,rrnaosldnki,N,gtec,")
                .header( "nonEmpty", "jm,O]4\"TGLs,dbgsm,$+W,qrqtb,true")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "width=-1,height=0,zcw=979.2,fuxolvzudzs=")
                .header( "nullable", "width,0,jpft,yQF;,hgadz,bfxhzepzm,~0#3%Afz,V8zq^R5,kenwfgrxfn,true,pky,-281.3,fvmbotsplcbhc,511")
                .header( "nonEmpty", "nivpwodyiqskt,rvtwhqqbye,`ud,b+6@,bpweeanzvkzvkznv,,pziwa,-404,a,dmzihzfldn,336.1,rjbnpra,-585,qydstkmy,-245.9,pduksjsdnmc,prrvwzdnvo,true,ennk,true,ek,P")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=utd,y2FLu,b?6>,uwrjnjqjterf=rpaofavp,,zupzuvae,505.6,r=true,tzqohf=811")
                .header( "nullable", "width,0,pxqkftpd,tam,true,nowmco,-245")
                .header( "nonEmpty", "avwuendppm,,jy,-419.2,jxhkfhxsztgrovf,sohzxxlvm,V")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "exploded", "height=-1,wig=-321,kezv=(t,aW_bpl,ztclbqx=1014.3")
                .header( "nullable", "width,0,xozlsldj,562,er,gtjthriog,811,jspesom,-571,loiqdig,a\"o")
                .header( "nonEmpty", "ias,500,yrivbs,wnnwkpywkvfcihw,true,rvyshimrnajkbvho,.G6U,g]vU,ghvvvjrjriswm,-165")
            .when()
                .request( "GET", "/header/object")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "'V[nyu0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwXgIZjb%WWDG:>2AvL#|L1:")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "8E;")
                .header( "empty", "u")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyValueLength_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", ">Dmy:o27HkDZ%rt)listU>?]sAviyah`46\"yhpb\"~'tm1ZV>& S{DOg6(3Wtz>n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg],I=BTHC6;vgsi}{1YsSiTi|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_o#)?iw1sv09rscxxp8%X/#>V|=KN(ip@[:+&rEEMcN.PyrD\\.9M5:</v2\\t|3I;&@@")
                .header( "empty", "~$JU@ihst_E[pP)^oQ)x4U1!` `FGJ+1Z~3\"P(Ys|!DI,F) >*$t{egzYY<JB](%E4N9BRTWHzX(0o|HhJW4M.w(:\"QB6?Nu^7M%pKSwr/BFl6m7V$v2vn mLDewSA>qQZ=`bnm^hT=[M~LM]2iAtQ*{M*H< yoM\\Z<]t")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "")
                .header( "nullableNonEmpty", "QO{7_`&pEdwsXr3Fgmd J| =o_oc=[`Pal#.G`.4{wREEbl'H<q%|EdyDlPp`z<rpJWEwMgaqlhIs6/-b9X9sg~x>>tB%,/k~ c\"V_Ak1T=Jco~ D\\X2>_qzE`@c% Kg,)xak\\Q:bcGrj6(F(g8\\'gX'uf8ui")
                .header( "nonEmpty", "~Y+")
                .header( "empty", "F")
            .when()
                .request( "GET", "/header/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "-X(mg(f3NDGZW$sU\\l,8dK4Ush*wg5cupL@=wy5R:7{*B}O$/dmfFfe^>quwQw]Cb_@R\"-+\"mX(o!+EUV<<>`=CBg2d\"Ow:e6KT6;IUFdOY}vLS&K8^S:F+,*V$`$p.VlD}[!xuGEGh_MV$i:26U'~lb<#Ctwr~#\\d8l= SP^A6>|V@<p idGE`zGK-[#'o/K*#@mPxlw4ejJJBF<m?msiX,SRO\\FRR`~6E'JU(8+l\"o! G-M3VX")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "UxHz~rHo&3mshX&U{Bu9J_GI'ekwfY})Fo{JuFa}]]Sd8$&x9_%B")
            .when()
                .request( "GET", "/header/string")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", ",Bu3p}9%{Q,2e>=R/-)D`0Q;@f_~3+2Dys=ohR+{T8*hW$:%2Z7Udz)")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "MKq)8$Zc\\DOqfvFSDLh@^")
                .header( "empty", "")
            .when()
                .request( "GET", "/header/string")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "{u{j>X+c+mz=(BJ`)Q%Y?C~r2Y$Q^:~]7kY~)2XReg230od3D2R%rXN$wvKQOG\"UBx{D|=")
                .header( "nullableNonEmpty", "")
                .header( "empty", "#U_+$fF/-~Wv^Ij3QRe]QgSxqqIFk\"Y[cn<w:EYv3qSUy>{V@DVZ#y?]")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "?H^YNRWo,V67l'D{8AvIQT;wrW_2bSG0gNzVEBzAVheOMT3RzN`fTSro.#'Ql*w'|x)7 i+Fs/fIf)U^d)oG\"y0ls;+4F0T?1&^fD}V]j-U~2g=dDBY2MZ{Kjd$>-T,V2Zpid~eSCkct=I%B2H_}OtD^*b}mMlhvG/h")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "")
                .header( "empty", "kgJ1|Rx^X<fl0>UO! -:}'OT7c(VNF8W6H@5HYFGoE`c@T\\Gq7oQ^LQ_kr{l~f-$mJSHA\\.TsZ#y,Rd>5V+#^f(Fn2!IV^~0]\\K#|k%mTt5y_bI&q:q1a ]DkTwZx7-AD3m,>56P~)=\\&QQsh0pC^0iZ][l9hgky;oY")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NonEmptyValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "RDl`UPTIE$A9=KuGLSrg}M.fErV+$@wZ$v7%D\\z5soZ<Hi|o^EIU!<rs+jt<}sEKs0:]x`Qv$S(P\\Q~Qkr]6L>}DXTz k$O,pt.z<#82?")
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", ";$")
                .header( "empty", "RwF$ VpxtPj\"aye\"\\g:hAWO#!{HloWxb$_`[x'0,m}qV$")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nonEmpty.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullableNonEmpty", "")
                .header( "nonEmpty", "\"|@WwWnU;KcSda5m\\l \"")
                .header( "empty", "Dt7<jmHGOIwc*Eb+3;L=Axqz`&Q?D%tIK]JG+/:?U/WIyoq\\.U`o'gMllr+uCfq[O5+\\~c!%U~WkmaNOR+^*I!`zy}pD<,A'3@3h\"|uXQNd8yCU)ow`xSOFxIfrc] 7vY.YGU|Kc'2e)_:uH$clo$xDn`nk$U$&b\"JPJ5YeDxMR%(]necb|EUa[1zWu\\2y$,D_@7&swhL9~o(XbW-U=gv2|**b6;A`zNrty}j!qHx")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getHeaderString_NullableNonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "nullable", "pyFgS=2'4!1H~Qi.X)p|`2#XkY[5{#_@&\\$<N]K=Q'F4#R*-j\\m\\j|j/3!ZfYWo{qEFoZ)DiMa(K/\\DW;u*j\"C?lsNptCBOuUr%$^|m=]z I3SY]ud.c3~SAY'D[yHHC'd)5x|Q4M*|:s;2C]6*W`hSOgR!WOv.`NVW~bmLQt;x")
                .header( "nonEmpty", "\"vuk *Tgu%?0VVt+\\{/hLP0GY/@xG=LFzsm;bVCvW/tcT8ViXXLfBR=eR6e]+kuUbl[\\nrPv/'!92NYcqkO(6dDZ9^~_!BA][Bx[[Q/YW9\\y:(MN*WazDVopn:{%!+H}aumWV^l*r$A)K@1r7Wx!hRflRfGvf9X3//C|6h:Ci,~Qyg\"r2:$S)#e3R^Lkg&8<-Xh%=x")
                .header( "empty", "\"r)FS/Z* ?o4wol8TB#J>UY]%+~vtHp$FXd`B0g4X|\\{K\\E/&sLj*!I7urAs13./#r#1,io.:N=YWu;b?(Z_cbh.G,MuF0XeE ~#!|/Ak<[~Jf W)HF9%I) `HR$pYUUgI*mIf`Ijesidzsv~'Y$rz3[q|BV\"N@~\\/d")
            .when()
                .request( "GET", "/header/string")
            .then()
                // nullableNonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/header/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/header/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-915629898")
                .pathParam( "nullable", "-777592863")
                .pathParam( "nonEmpty", "-787864396,968195964,-941125629")
                .pathParam( "empty", "-479634177")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", ",-290067183,-159033827,-879373065,-998839893,1007387311,840979781,718184765,-719901843,212473142,-466899496,608407672,-599674523")
                .pathParam( "nullable", ",-660351823,-362664233")
                .pathParam( "nonEmpty", ",226034055,-884703973,226034055")
                .pathParam( "empty", ",-97868370,-279296209,485709141,-216666114,-453603517,-935803584,963242121")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "0")
                .pathParam( "nullable", "0")
                .pathParam( "nonEmpty", "0,-790520378,464352219")
                .pathParam( "empty", "0")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "247662958,206516015,206516015")
                .pathParam( "nullable", "339122953,339122953,-164249220")
                .pathParam( "nonEmpty", "597556810,-739813784,-10240067,47029742,757335484,822581821,-956279549,220628827,-858529578,895300504,-739813784,722180891,170367393,683319538,703520610")
                .pathParam( "empty", "117826646,117826646")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-1044207262")
                .pathParam( "nullable", "-210497736")
                .pathParam( "nonEmpty", "-779353904,665084706,396090616")
                .pathParam( "empty", "")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-581166028")
                .pathParam( "nullable", "-613571985")
                .pathParam( "nonEmpty", "-551185881,168692740,-987473178")
                .pathParam( "empty", "true")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-777374064")
                .pathParam( "nullable", "-365727516")
                .pathParam( "nonEmpty", "-527133846,772732844,-328103540")
                .pathParam( "empty", "-213.4")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-213474350")
                .pathParam( "nullable", "-480690895")
                .pathParam( "nonEmpty", "")
                .pathParam( "empty", "-449024820")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-194716814")
                .pathParam( "nullable", "-978628238")
                .pathParam( "nonEmpty", "-545.9")
                .pathParam( "empty", "-448965344")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-513574421")
                .pathParam( "nullable", "-272279337")
                .pathParam( "nonEmpty", "-752797874,-752797874")
                .pathParam( "empty", "-621459142")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-728126616")
                .pathParam( "nullable", "-321574503")
                .pathParam( "nonEmpty", "hnd,909.0,edrwghygiso,764.4,485495228,998536309")
                .pathParam( "empty", "-791141610")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-41580670")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "-732336218,109949038,-1044123185")
                .pathParam( "empty", "-1027669540")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-502403854")
                .pathParam( "nullable", "mgfpotex,:/3GOUK,5L\"y^5a,@3O-;,mzfuknzqlju,315.3,gmbpaka,-970")
                .pathParam( "nonEmpty", "-221863041,-461472705,-748673657")
                .pathParam( "empty", "-521799715")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "-7668570")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "-267637067,-164428690,-823710164")
                .pathParam( "empty", "-882107883")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "")
                .pathParam( "nullable", "-448547311")
                .pathParam( "nonEmpty", "-83797675,-223882716,894636233")
                .pathParam( "empty", "-781422867")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "cwmpc=T.n+,sfx=true,vivam=-352")
                .pathParam( "nullable", "-533890351")
                .pathParam( "nonEmpty", "-734352658,-445233832,1010022565")
                .pathParam( "empty", "-171266125")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathArrayEmptyNonEmptyNullableExploded_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "true")
                .pathParam( "nullable", "-1045116957")
                .pathParam( "nonEmpty", "-955173419,636208010,796706572")
                .pathParam( "empty", "-224751716")
            .when()
                .request( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/array/{empty}/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0,height=0,mukwskokuxrpxznd=AEm+ET")
                .pathParam( "nullable", "width,0,height,0,pkjqzodjr,ouuw,true")
                .pathParam( "nonEmpty", "width,0,height,0,bzdjmkakkdsyxir,true,hglysqv,g,713,sjqeocslwsgot,-744.1,opaqfmjfbyjuwejt, MJd0T$m")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=")
                .pathParam( "nullable", "width,")
                .pathParam( "nonEmpty", "height,")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "height=,xrqcwtokegnltvfe=true,znltljqlwt=388.1,kxjjhfiewfmpapz=p>N0d")
                .pathParam( "nullable", "height,,fdhvxpppi,820,r,qI^e,4Hd@2,2,cf,w RJ=\"8")
                .pathParam( "nonEmpty", "width,,qnwnhrx,true,ioruzhsoollf,{C,2,OK")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=275788551")
                .pathParam( "nullable", "width,153392868")
                .pathParam( "nonEmpty", "width,221713725,height,748151146")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "height=278892660,uyrgjiuva=#I/*,igZ6Gm,&Pm%` %D,dilytxpeioj=429.7")
                .pathParam( "nullable", "height,100984062,aocantftydravpss,,9,xzaqqluhcw,true")
                .pathParam( "nonEmpty", "vrjudxctf,KQjPAF_")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "724")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,HOD7y],height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,-1,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,#C")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,-1")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "-482")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,{A")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,-1")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0,height,240.7")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0")
                .pathParam( "nullable", "width,0,height,-1")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "517")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=T1J,,#")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=-1")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0,height=-76.4")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathObjectNonEmptyNullableExploded_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "exploded", "width=0,height=-1")
                .pathParam( "nullable", "width,0")
                .pathParam( "nonEmpty", "width,0,height,0")
            .when()
                .request( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/object/{nonEmpty}/{nullable}/{exploded}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "A")
                .pathParam( "nonEmpty", "S0)")
                .pathParam( "empty", "$")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyValueLength_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "nullable", "hhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wzx6{<h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=")
                .pathParam( "nonEmpty", "\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*aG)@b&1avN")
                .pathParam( "empty", "IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^")
            .when()
                .request( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_EmptyType_Is_Null() {
        Response response =
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
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyType_Is_Null() {
        Response response =
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
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NonEmptyValueLength_Is_2() {
        Response response =
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
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPathStringEmptyNonEmptyNullable_NullableType_Is_Null() {
        Response response =
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
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/path/string/{empty}/{nonEmpty}/{nullable}", response.statusCode(), responseHeaders( response));
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

    private static Map<String,List<String>> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( groupingBy( Header::getName, mapping( Header::getValue, toList())));
    }
}
