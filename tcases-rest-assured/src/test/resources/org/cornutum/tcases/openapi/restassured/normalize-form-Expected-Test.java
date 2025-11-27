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

public class NormalizeFormTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getCookieArray_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-565342359")
                .cookie( "nonEmpty", "-448786693")
                .cookie( "nonEmpty", "-58817267")
                .cookie( "empty", "")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-781624554")
                .cookie( "nullable", null)
                .cookie( "nonEmpty", null)
                .cookie( "nonEmpty", "-953241066")
                .cookie( "nonEmpty", "718111172")
                .cookie( "nonEmpty", "-189482066")
                .cookie( "nonEmpty", "-577106341")
                .cookie( "nonEmpty", "-405224805")
                .cookie( "nonEmpty", "39267938")
                .cookie( "nonEmpty", "-526959074")
                .cookie( "nonEmpty", "-526959074")
                .cookie( "nonEmpty", "38707495")
                .cookie( "nonEmpty", "65911585")
                .cookie( "empty", "-333725341")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", null)
                .cookie( "exploded", "-289082076")
                .cookie( "exploded", "428615712")
                .cookie( "exploded", "-457086654")
                .cookie( "exploded", "54264425")
                .cookie( "exploded", "-569909011")
                .cookie( "exploded", "-885726431")
                .cookie( "exploded", "1036773847")
                .cookie( "exploded", "-42857965")
                .cookie( "exploded", "-250682074")
                .cookie( "nullable", "-32832183")
                .cookie( "nonEmpty", "0")
                .cookie( "nonEmpty", "-3388464")
                .cookie( "nonEmpty", "226666727")
                .cookie( "empty", null)
                .cookie( "empty", "10972596")
                .cookie( "empty", "-521795181")
                .cookie( "empty", "885610005")
                .cookie( "empty", "560695496")
                .cookie( "empty", "24424302")
                .cookie( "empty", "-826273549")
                .cookie( "empty", "150436118")
                .cookie( "empty", "-550002744")
                .cookie( "empty", "-22302229")
                .cookie( "empty", "-920525169")
                .cookie( "empty", "431444732")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "")
                .cookie( "nullable", null)
                .cookie( "nullable", "973248429")
                .cookie( "nullable", "536437959")
                .cookie( "nullable", "-584389915")
                .cookie( "nullable", "336526589")
                .cookie( "nonEmpty", "1016279133")
                .cookie( "nonEmpty", "531632009")
                .cookie( "nonEmpty", "700989698")
                .cookie( "nonEmpty", "700989698")
                .cookie( "nonEmpty", "303537315")
                .cookie( "nonEmpty", "-812539917")
                .cookie( "empty", "0")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "0")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-562079385")
                .cookie( "nonEmpty", "174026061")
                .cookie( "nonEmpty", "-1056637663")
                .cookie( "empty", "1053746263")
                .cookie( "empty", "178857939")
                .cookie( "empty", "-1028087847")
                .cookie( "empty", "122749270")
                .cookie( "empty", "220026930")
                .cookie( "empty", "178857939")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NullableItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "1020857417")
                .cookie( "exploded", "-704671765")
                .cookie( "exploded", "-1013048295")
                .cookie( "exploded", "-23834465")
                .cookie( "exploded", "-679597483")
                .cookie( "exploded", "-386655230")
                .cookie( "exploded", "-625881874")
                .cookie( "exploded", "439773803")
                .cookie( "exploded", "-548330101")
                .cookie( "exploded", "168939545")
                .cookie( "exploded", "414745963")
                .cookie( "exploded", "-550623962")
                .cookie( "exploded", "70540567")
                .cookie( "exploded", "764181425")
                .cookie( "exploded", "-233718547")
                .cookie( "exploded", "-386655230")
                .cookie( "nullable", "0")
                .cookie( "nonEmpty", "0")
                .cookie( "nonEmpty", "1043878023")
                .cookie( "nonEmpty", "-831936222")
                .cookie( "nonEmpty", "904738804")
                .cookie( "nonEmpty", "-769739125")
                .cookie( "nonEmpty", "-150089467")
                .cookie( "nonEmpty", "-769739125")
                .cookie( "empty", "")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NullableItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "")
                .cookie( "nullable", "153911627")
                .cookie( "nullable", "-683480179")
                .cookie( "nullable", "3109083")
                .cookie( "nullable", "-944078301")
                .cookie( "nullable", "-921254517")
                .cookie( "nullable", "525990732")
                .cookie( "nullable", "-31450784")
                .cookie( "nullable", "694887281")
                .cookie( "nullable", "-944078301")
                .cookie( "nullable", "-637438237")
                .cookie( "nullable", "-460963866")
                .cookie( "nullable", "-822524327")
                .cookie( "nullable", "-103723875")
                .cookie( "nullable", "-614937415")
                .cookie( "nullable", "545765079")
                .cookie( "nullable", "-872814856")
                .cookie( "nonEmpty", "504448473")
                .cookie( "nonEmpty", "937711258")
                .cookie( "nonEmpty", "465531090")
                .cookie( "empty", "-327281778")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-1018827834")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-916832479")
                .cookie( "nonEmpty", "137985347")
                .cookie( "nonEmpty", "-916832479")
                .cookie( "nonEmpty", "-157367200")
                .cookie( "nonEmpty", "718103085")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-996603262")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-591357296")
                .cookie( "nonEmpty", "11613030")
                .cookie( "nonEmpty", "847594523")
                .cookie( "nonEmpty", "22216678")
                .cookie( "nonEmpty", "11613030")
                .cookie( "nonEmpty", "-1068138668")
                .cookie( "nonEmpty", "526934627")
                .cookie( "nonEmpty", "-632985551")
                .cookie( "nonEmpty", "-754389747")
                .cookie( "nonEmpty", "-734061933")
                .cookie( "nonEmpty", "608104576")
                .cookie( "nonEmpty", "-612049959")
                .cookie( "nonEmpty", "-568846719")
                .cookie( "empty", null)
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-344800145")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-338067524")
                .cookie( "nonEmpty", "205428750")
                .cookie( "nonEmpty", "890409189")
                .cookie( "nonEmpty", "98468685")
                .cookie( "nonEmpty", "-317856439")
                .cookie( "nonEmpty", "-486576637")
                .cookie( "nonEmpty", "-364923976")
                .cookie( "nonEmpty", "-743256754")
                .cookie( "nonEmpty", "-317856439")
                .cookie( "nonEmpty", "588622925")
                .cookie( "nonEmpty", "778707242")
                .cookie( "empty", "519")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-561497123")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-352462576")
                .cookie( "nonEmpty", "-785327029")
                .cookie( "nonEmpty", "-14390459")
                .cookie( "nonEmpty", "-495103742")
                .cookie( "nonEmpty", "-1012187196")
                .cookie( "nonEmpty", "503551354")
                .cookie( "nonEmpty", "-775752781")
                .cookie( "nonEmpty", "19030395")
                .cookie( "nonEmpty", "-1010416716")
                .cookie( "nonEmpty", "35311021")
                .cookie( "nonEmpty", "-859205831")
                .cookie( "nonEmpty", "923613126")
                .cookie( "nonEmpty", "19030395")
                .cookie( "nonEmpty", "-456476596")
                .cookie( "empty", "YM")
                .cookie( "empty", "-489030819")
                .cookie( "empty", "-794299005")
                .cookie( "empty", "476122851")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-563029178")
                .cookie( "nullable", "")
                .cookie( "empty", "0")
                .cookie( "empty", "-209376627")
                .cookie( "empty", "-512227688")
                .cookie( "empty", "856247782")
                .cookie( "empty", "-979798962")
                .cookie( "empty", "572291610")
                .cookie( "empty", "-872192705")
                .cookie( "empty", "-653583394")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-218610464")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", null)
                .cookie( "empty", "0")
                .cookie( "empty", "979139966")
                .cookie( "empty", "-224006432")
                .cookie( "empty", "95229284")
                .cookie( "empty", "576868268")
                .cookie( "empty", "476564567")
                .cookie( "empty", "337238062")
                .cookie( "empty", "-835453159")
                .cookie( "empty", "772638228")
                .cookie( "empty", "881389498")
                .cookie( "empty", "-977897282")
                .cookie( "empty", "-35463229")
                .cookie( "empty", "28865176")
                .cookie( "empty", "795751477")
                .cookie( "empty", "49037830")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-407934039")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-170.5")
                .cookie( "empty", "0")
                .cookie( "empty", "609062582")
                .cookie( "empty", "740548562")
                .cookie( "empty", "414926909")
                .cookie( "empty", "-109131844")
                .cookie( "empty", "837573760")
                .cookie( "empty", "556716078")
                .cookie( "empty", "-42024328")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-294542113")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-522931046")
                .cookie( "nonEmpty", "-522931046")
                .cookie( "empty", "0")
                .cookie( "empty", "-573103983")
                .cookie( "empty", "-528255283")
                .cookie( "empty", "-252051861")
                .cookie( "empty", "-998225438")
                .cookie( "empty", "770576130")
                .cookie( "empty", "863074852")
                .cookie( "empty", "100910611")
                .cookie( "empty", "235418093")
                .cookie( "empty", "265905143")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-851546344")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "true")
                .cookie( "nonEmpty", "-682044641")
                .cookie( "nonEmpty", "880893486")
                .cookie( "nonEmpty", "-993779903")
                .cookie( "nonEmpty", "-682044641")
                .cookie( "nonEmpty", "91079777")
                .cookie( "empty", "0")
                .cookie( "empty", "8662297")
                .cookie( "empty", "534406386")
                .cookie( "empty", "-216024918")
                .cookie( "empty", "-373673839")
                .cookie( "empty", "-48202881")
                .cookie( "empty", "468616366")
                .cookie( "empty", "-560042001")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-908920109")
                .cookie( "nonEmpty", "-274691649")
                .cookie( "nonEmpty", "-357591821")
                .cookie( "nonEmpty", "-637190374")
                .cookie( "nonEmpty", "1039405479")
                .cookie( "nonEmpty", "209082601")
                .cookie( "nonEmpty", "-506819335")
                .cookie( "nonEmpty", "346111683")
                .cookie( "nonEmpty", "-273424820")
                .cookie( "nonEmpty", "-793508738")
                .cookie( "nonEmpty", "-793508738")
                .cookie( "nonEmpty", "626750076")
                .cookie( "nonEmpty", "476827660")
                .cookie( "nonEmpty", "-270683240")
                .cookie( "nonEmpty", "-477115887")
                .cookie( "nonEmpty", "-681794279")
                .cookie( "empty", "0")
                .cookie( "empty", "-553864973")
                .cookie( "empty", "-518374648")
                .cookie( "empty", "252074339")
                .cookie( "empty", "323476160")
                .cookie( "empty", "-542414339")
                .cookie( "empty", "-20870392")
                .cookie( "empty", "-84522503")
                .cookie( "empty", "424562319")
                .cookie( "empty", "519691877")
                .cookie( "empty", "-366177198")
                .cookie( "empty", "-30229074")
                .cookie( "empty", "-185994301")
                .cookie( "empty", "-93407262")
                .cookie( "empty", "986116516")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-695451959")
                .cookie( "furzjarlzcfetyv", "-717.6")
                .cookie( "fistxx", ",H-d&'6xc")
                .cookie( "peus", "-324")
                .cookie( "nonEmpty", "-925962157")
                .cookie( "nonEmpty", "-612820414")
                .cookie( "nonEmpty", "-941231916")
                .cookie( "nonEmpty", "-915440650")
                .cookie( "nonEmpty", "-915440650")
                .cookie( "empty", "0")
                .cookie( "empty", "-667445592")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "-768875510")
                .cookie( "nullable", "SB")
                .cookie( "nonEmpty", "-731366998")
                .cookie( "nonEmpty", "969828757")
                .cookie( "nonEmpty", "1027294977")
                .cookie( "nonEmpty", "-731366998")
                .cookie( "nonEmpty", "621143402")
                .cookie( "nonEmpty", "-1023449552")
                .cookie( "empty", "0")
                .cookie( "empty", "-937997398")
                .cookie( "empty", "538837620")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-96405917")
                .cookie( "nonEmpty", "3871894")
                .cookie( "nonEmpty", "-953053195")
                .cookie( "nonEmpty", "-386868218")
                .cookie( "nonEmpty", "-147547458")
                .cookie( "nonEmpty", "828808264")
                .cookie( "nonEmpty", "-953053195")
                .cookie( "nonEmpty", "-51187391")
                .cookie( "nonEmpty", "-143823736")
                .cookie( "nonEmpty", "575919560")
                .cookie( "nonEmpty", "892177005")
                .cookie( "nonEmpty", "-518936275")
                .cookie( "empty", "0")
                .cookie( "empty", "-477657740")
                .cookie( "empty", "-307607461")
                .cookie( "empty", "-731939627")
                .cookie( "empty", "-67655132")
                .cookie( "empty", "892480166")
                .cookie( "empty", "360542270")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", null)
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-1048081443")
                .cookie( "nonEmpty", "-833381957")
                .cookie( "nonEmpty", "1002902123")
                .cookie( "nonEmpty", "128157472")
                .cookie( "nonEmpty", "-400413433")
                .cookie( "nonEmpty", "44208217")
                .cookie( "nonEmpty", "631592114")
                .cookie( "nonEmpty", "982733964")
                .cookie( "nonEmpty", "48092155")
                .cookie( "nonEmpty", "-565919188")
                .cookie( "nonEmpty", "1002902123")
                .cookie( "empty", "0")
                .cookie( "empty", "-660972810")
                .cookie( "empty", "32064568")
                .cookie( "empty", "1041949967")
                .cookie( "empty", "168680011")
                .cookie( "empty", "-891953816")
                .cookie( "empty", "43902030")
                .cookie( "empty", "139133801")
                .cookie( "empty", "-817331049")
                .cookie( "empty", "491801863")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "10")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-1050781376")
                .cookie( "nonEmpty", "-624145675")
                .cookie( "nonEmpty", "-92584419")
                .cookie( "nonEmpty", "-1004532526")
                .cookie( "nonEmpty", "-635084753")
                .cookie( "nonEmpty", "-353898474")
                .cookie( "nonEmpty", "225828396")
                .cookie( "nonEmpty", "-802138809")
                .cookie( "nonEmpty", "965616434")
                .cookie( "nonEmpty", "-538186609")
                .cookie( "nonEmpty", "-1059221424")
                .cookie( "nonEmpty", "-1050781376")
                .cookie( "empty", "0")
                .cookie( "empty", "-925023055")
                .cookie( "empty", "457166502")
                .cookie( "empty", "94612162")
                .cookie( "empty", "-827827377")
                .cookie( "empty", "-428233039")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieArray_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "exploded", "uclexcaqeluv,-572.7,ogftmahijm,%+fBh")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-439089015")
                .cookie( "nonEmpty", "-301211487")
                .cookie( "nonEmpty", "-439089015")
                .cookie( "nonEmpty", "-934989059")
                .cookie( "empty", "0")
                .cookie( "empty", "-679036693")
                .cookie( "empty", "-1054625596")
                .cookie( "empty", "-586895926")
                .cookie( "empty", "165486793")
                .cookie( "empty", "-669421458")
                .cookie( "empty", "-570048323")
                .cookie( "empty", "-648671574")
            .when()
                .request( "GET", "/cookie/array")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[width]", "0")
                .cookie( "deep[height]", "0")
                .cookie( "deep[mnxehrzvrc]", ".yKODlD_")
                .cookie( "width", "0")
                .cookie( "height", "0")
                .cookie( "nxzlykzutypeulgh", "")
                .cookie( "busaicy", "")
                .cookie( "xwo", "-62.9")
                .cookie( "nullable", "")
                .cookie( "width", "0")
                .cookie( "height", "0")
                .cookie( "esohqyqdpssoqss", "-228")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[width]", null)
                .cookie( "width", null)
                .cookie( "nullable", null)
                .cookie( "height", null)
            .when()
                .request( "GET", "/cookie/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", null)
                .cookie( "deep[wuh]", "-472.7")
                .cookie( "deep[sjwovaykrnvzmcl]", "-150")
                .cookie( "deep[g]", "-899")
                .cookie( "height", null)
                .cookie( "ruoxrmsf", "841")
                .cookie( "nullable", "width|0|height|0|tsnmciauydpvv|765.7|isfghciepiwkj|xhkpkbcb,true|nyroo|yV<Q0=E[")
                .cookie( "width", null)
                .cookie( "chvjffybrobuea", "FSM5UJF")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[width]", "544258710")
                .cookie( "width", "953055855")
                .cookie( "nullable", "width|")
                .cookie( "width", "65944255")
                .cookie( "height", "911040642")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "253819245")
                .cookie( "deep[egibiyycrbbbsyvo]", "11.0")
                .cookie( "deep[kcfckrivesrgf]", "true")
                .cookie( "deep[slr]", "292.3")
                .cookie( "height", "801149319")
                .cookie( "rbyqfnu", "-175.8")
                .cookie( "nullable", "width|292509355|height||fxvq|true|vozdcudalhfe|zuliplqfhtx,-855.0,qupqdhyoil,480.4,awgmkytwrfarmqm,-214.7|xlyidypivznlif|Wi>T")
                .cookie( "gesbmxt", "true")
                .cookie( "lxryvgsmv", "894.3")
                .cookie( "dxocql", "1,8A?`")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[width]", "0")
                .cookie( "width", "0")
                .cookie( "nullable", "height|581389088")
                .cookie( "width", "0")
                .cookie( "height", "0")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[iuqbvbnyl]", "")
                .cookie( "deep[myboywusicd]", ",3?-ic,RV.@")
                .cookie( "height", "0")
                .cookie( "adtrwupwy", "be)::")
                .cookie( "ifpgtvn", "725")
                .cookie( "rclhjuzjqs", "vs,627,kmvqxrdjgt,true")
                .cookie( "nullable", "width|0|vhszudaw|800.2")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[bvo]", "12")
                .cookie( "deep[rxkedb]", "*)Fa8M]2")
                .cookie( "deep[drulzxkylqaygbqz]", "102.2")
                .cookie( "height", "0")
                .cookie( "bhsygfdgawycgwl", "tje6q")
                .cookie( "nullable", "width|0|bnivrf|980.8")
                .cookie( "nonEmpty", null)
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[houtcmnjymupm]", "646")
                .cookie( "height", "0")
                .cookie( "kqapkfsel", "1018.0")
                .cookie( "nullable", "width|0|jthfeimgqj|-80.4|hchj|'")
                .cookie( "nonEmpty", ":|ZX")
                .cookie( "nonEmpty", "P3aO_")
                .cookie( "nonEmpty", ".DF$xr9")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[lbhl]", "t1z!o")
                .cookie( "deep[jrbrzdiqeogno]", "true")
                .cookie( "height", "0")
                .cookie( "enteol", "true")
                .cookie( "ko", "eT2bGe")
                .cookie( "nullable", "width|0|rhcszzagu|-752|bkazfhsrvfwq|")
                .cookie( "width", "4kYl8")
                .cookie( "dnttnjvxu", "pjggu,-391,uz,GbdvXsz,wvvzo,true")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[bzlpukdcemvosuvn]", ".)")
                .cookie( "deep[fzovbuwwc]", "-805")
                .cookie( "deep[tvntxmvj]", "true")
                .cookie( "height", "0")
                .cookie( "nchhbcmdnlsh", "342")
                .cookie( "hlprungyaz", "N7G")
                .cookie( "vyponitt", "sM2b")
                .cookie( "nullable", "width|0|cgxjgnll|NR*2mJ&D|uqqchxtkpf|ezyrocuxnauccukc,557.2,dnpb,-567.7|vtpmxrgibevi|cz,true,knyexorbxchklbag,499.4")
                .cookie( "width", "-1")
                .cookie( "uqq", "")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[tihrk]", "-746")
                .cookie( "height", "0")
                .cookie( "qpodymdqfc", "")
                .cookie( "bmee", "944")
                .cookie( "nullable", "width|0|ptconycj|-728|up|H:sm`Kpf")
                .cookie( "height", "886.4")
                .cookie( "jfpfqjs", "ysqjulkkyjsd,")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[ulxvpgknbpykwn]", "-943")
                .cookie( "deep[vilq]", "true")
                .cookie( "height", "0")
                .cookie( "ffbvhbdrsm", "*k~9EN")
                .cookie( "q", "epkssurjtrnsu,L|#h02),-|l")
                .cookie( "thamamnhul", "596.6")
                .cookie( "nullable", "width|0|qzgcmypcsct|true")
                .cookie( "height", "-1")
                .cookie( "smykgxtgj", "")
                .cookie( "rc", "-7z-e@")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[xjxnjyvqmtocpufz]", "pbfmprnigjjp,461,caannwoanhe,0|4_GP,sexdwaeixjt,316")
                .cookie( "height", "0")
                .cookie( "skfgz", "-865.0")
                .cookie( "rgbfgtlhbcdycw", "yiiyrmsa,true,sdyxuph,723.6,hliqdozpo,")
                .cookie( "mafjindgho", "true")
                .cookie( "zkmaqa", "996")
                .cookie( "rkz", "true")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[kvgosvrvhpa]", "I4nG,$iDH1ea,")
                .cookie( "deep[s]", "ttaomjplrzx,405,hbeh,70.5")
                .cookie( "height", "0")
                .cookie( "s", "-975")
                .cookie( "nullable", "true")
                .cookie( "w", "PE'_Q~X")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[aknahxuuawfks]", "tvbenftoxmqdf,NMnz,eseynvlornzsq,>-H")
                .cookie( "deep[hqlbrgbgohofe]", "true")
                .cookie( "height", "0")
                .cookie( "epw", "$IDQL")
                .cookie( "cpxlpuhytigshgcw", "")
                .cookie( "ovnmqlm", "-478")
                .cookie( "nullable", "width|2.~!MN-`|v|true")
                .cookie( "dgromuloskb", "496.3")
                .cookie( "blvfdpic", "BZT[")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[uhgllutvgzjpgdf]", "354.9")
                .cookie( "height", "0")
                .cookie( "zubejuohaakr", "!m`0|i,.zrJ,L8")
                .cookie( "nullable", "width|-1|cjgyhzprmalewn|512|nyaewxyfitslnpv|")
                .cookie( "ecxpfmbbf", "xmdpbiknsqz,true")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[hyn]", "740")
                .cookie( "height", "0")
                .cookie( "qzgdqsrqrvjuy", "_R]ZeNj,9<@DU(n")
                .cookie( "nullable", "width|0|height|jxgqs,S(#:<1T,wgjkwhtyvijdvagz,-813,hjwyjlfasjgs,true|biis|e/6|jghxeuaqevjme|~^GN|av|e")
                .cookie( "mxeyee", "true")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[uulygozfw]", "AuYtJ,OBJ,:tiKJRI{")
                .cookie( "height", "0")
                .cookie( "osqw", "Pb-wI")
                .cookie( "zjssoye", "442.1")
                .cookie( "nullable", "width|0|height|-1|bsbqild|-402.2|gtvpatbrk|kkxjjruosd,true,wmuuiuyqifei,],xmazyajj,true")
                .cookie( "szvkkedwdijigqv", "841")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[yczgdkbpoibv]", "262")
                .cookie( "nullable", "width|0|hdxgihaftwiylwp|yomatsvo,,usalbznkwd,|vld|C1@")
                .cookie( "jfx", "-911")
                .cookie( "rmxnfyy", "true")
                .cookie( "qxrxkncehm", "j2,o!P{'mk,")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[zblflmaujpcoi]", "true")
                .cookie( "deep[rlsgharb]", ">KjQw6-{")
                .cookie( "exploded", null)
                .cookie( "nullable", "width|0|vbhbeyvn|true")
                .cookie( "eye", "tdhou,-670.6")
                .cookie( "nlnerilby", "<X?^?")
                .cookie( "ddoaujth", "iplvnpcp,true,ynhuppawuinpmtbi,?F[_'~r,lkeucsgmse,true")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[jnysvttvmbd]", "-370")
                .cookie( "exploded", "-54")
                .cookie( "nullable", "width|0|beh|true|etrbym|zzimed,true")
                .cookie( "ch", "-454.3")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[zesckuovoynp]", "-234.8")
                .cookie( "deep[avpxy]", "-310.5")
                .cookie( "deep[fplneff]", "true")
                .cookie( "width", "hqxuoxjl,-540.9")
                .cookie( "height", "0")
                .cookie( "uikqx", "83W")
                .cookie( "nullable", "width|0|jlapvoiukqlidkh||qldcgbpzhriyh|ivgvvpssheiluwbn,866.7,hulgknyacmq,Uj<E0c|xaioihlaxtv|-915")
                .cookie( "zmbeuu", "lqgywl,true")
                .cookie( "v", "723")
                .cookie( "wjjhefgrry", "uktqswwywcaqu,gQ#Lu,cuxtnsdenodk,true,jsxakfeirvrkhdh,KVwQUt)")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[sxwbrereag]", "472")
                .cookie( "width", "-1")
                .cookie( "height", "0")
                .cookie( "tfmlpyz", "-634.6")
                .cookie( "nullable", "width|0|npjxuylfhbnchzwt|true")
                .cookie( "yclpvtmtfdj", "-83")
                .cookie( "hqdfdpvrwsz", "")
                .cookie( "kpzcuolbx", "865")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[znyvjs]", "true")
                .cookie( "deep[vr]", "310.6")
                .cookie( "height", "cfrmj,E")
                .cookie( "awualpmklx", "jydjrgh,209.6,upp,73.8")
                .cookie( "iihji", "}<5|zgE")
                .cookie( "pen", "-424")
                .cookie( "nullable", "width|0|vv|!o/3E^OY|wifv|ktxzu,-336.4|yjgczpyctcpsgpnc|LoqP")
                .cookie( "kpnismixk", "X@V8{^.")
                .cookie( "rsbrb", "")
                .cookie( "vqmrlvj", "?2)V")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "0")
                .cookie( "deep[xhn]", "jpynptjwmevauj,YjyC(,nYfr:z,wulctyvgvlwwudf,,wojzzqlfjju,")
                .cookie( "height", "-1")
                .cookie( "hwbowwpvdgeedege", "671.6")
                .cookie( "nullable", "width|0|kdsdrrdugu|true|bvkncikulyo|611.7|qqukgvui|-417")
                .cookie( "emwv", "")
                .cookie( "kunukmfyj", "true")
                .cookie( "gtvb", "R.'")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_DeepDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "height", "0")
                .cookie( "ga", "-349")
                .cookie( "crzfupdnils", "284")
                .cookie( "gh", "699.0")
                .cookie( "nullable", "width|0|souhuzd|true|ly|true|o|7%a")
                .cookie( "uodj", "true")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // deep.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_DeepType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep", null)
                .cookie( "height", "0")
                .cookie( "qkzbjxd", "-619.5")
                .cookie( "nullable", "width|0|fvkmlbdaadwlvmi|OZ2q8|aot|true|mnblfocjjn|]4Ue1}`9,->r0!")
                .cookie( "jruv", "bqqupw,-532.2,ogftk,-799,lfb,true")
                .cookie( "km", "920.2")
                .cookie( "l", "")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // deep.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_DeepType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep", "-861.4")
                .cookie( "height", "0")
                .cookie( "etrtdvfughutwrcl", "true")
                .cookie( "nullable", "width|0|l|&icP")
                .cookie( "aeekmmemqlf", "8!E#vn>E")
                .cookie( "emgnfl", "true")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // deep.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_DeepValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[width]", "387.1")
                .cookie( "deep[height]", "0")
                .cookie( "deep[hpacqbal]", "L:c")
                .cookie( "deep[dzb]", "187.9")
                .cookie( "height", "0")
                .cookie( "x", "-418")
                .cookie( "yogcxktehzv", "true")
                .cookie( "nullable", "width|0|uxkphj|-153|tvnpzpej|*e|wbyzh|")
                .cookie( "ubgg", "Q(")
                .cookie( "qaqweaoaoilsb", "")
                .cookie( "xdmjtrc", "180.4")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // deep.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_DeepValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[width]", "-1")
                .cookie( "deep[height]", "0")
                .cookie( "deep[eomjevegkl]", "rluqxvv,Xl,yqoqyr,,nqdxphhuxzonlxs,o,+AYyF.,oy?")
                .cookie( "deep[igenog]", "true")
                .cookie( "deep[kkn]", "true")
                .cookie( "height", "0")
                .cookie( "yexpaodgbhvbthb", "")
                .cookie( "cccrjpcn", "9X")
                .cookie( "nullable", "width|0|tuppysoldatj|true|bofvxwg|true")
                .cookie( "rfxdxepey", "RS:#V,yi:Uhw:")
                .cookie( "shixkhkpqin", "-186")
                .cookie( "wh", "G3H3")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // deep.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_DeepValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "(F!:+")
                .cookie( "deep[mlh]", "188")
                .cookie( "deep[yf]", "Rugh-?r")
                .cookie( "height", "0")
                .cookie( "y", "zqxpjlovmhm,-302,qrtwpwqfsqsaovj,-212.5,zjakkf,")
                .cookie( "k", "uyxlbrtbbw,p$3:0R,qgjtrxfxvhozgiqr,-352.0,nqxzer,true")
                .cookie( "nullable", "width|0|kesywzpjdebfkips|383.3")
                .cookie( "hf", "t")
                .cookie( "ursr", "837")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // deep.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieObject_DeepValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "deep[height]", "-1")
                .cookie( "deep[z]", "1c072MOh")
                .cookie( "deep[atdxdfrahdy]", "uaaq,904.5,rdnooeg,PPi+Ef,kojtfbztlnoslgq,true")
                .cookie( "height", "0")
                .cookie( "nky", "}[PL")
                .cookie( "smixndpmzb", "")
                .cookie( "uoshigowpgvv", "-700")
                .cookie( "nullable", "width|0|liqydb|+Q")
                .cookie( "fxnvdzl", "true")
                .cookie( "jpmkln", "X,")
            .when()
                .request( "GET", "/cookie/object")
            .then()
                // deep.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "/l_/b^0Ho7c1ICX%i@kZAb-4O!WMRR^lV:7)TH+ISlE<P2YnpN5OF7?}?s9Qj}Gq&6n@39(b<otAk`gk-f*c2Jg%zlS+RC)JNL@~uKe^YtI)}:6U_xYCv3=vJr*bj.0pFU#a)-F>pC_Ns+axROFD}-|}_4~{z!ze4yIG8wr:g0#w<zjM}v~=VBFA")
                .cookie( "nonEmpty", ")cM")
                .cookie( "empty", "=DA|AW8y/4npnzl*dUaMY>6r0n.0_)!7vd[wd_5FrOq/]{4a2}/u3h>F$+{K9/.z.HbOek2po*@)J|$<Vu/NW&j^T^OT0c92Pw#mB|.#236M&sp]4^3zo.g0)A5<c9ZBWzi/w?6L<$`~y5jlt}jkua7pS80ZRHM~&w6|Z[<(W%gi8DvtJGP1D:29Cc4Sv.ik0a(Z'iOmVw<NjqXh-ib|Bb:XOZ*{[Z!QR/]TWK9uo.rJF2lVs}^w9QtR")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_EmptyValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", null)
                .cookie( "nonEmpty", "lMHS^mZ9uG7UJPtYTycAiG^|S*DA!6qtnDk&0^fN5/q9PEN=:&_jpq.~4UFiH0{6VnuTI@.V!tYm]J/RIt*G44X/`{4^/kW?`$4uKw>e?b3FJbJ8[NwMdobxrsLMpOU=Aw-RogUY_/")
                .cookie( "empty", "")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_NullableValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "B7n")
                .cookie( "empty", "-Y&b^vpoFTQ.|YS!Q6fp/s=tKdqu@>&<j}52#>-O6^Y26%A_EAMFLXAid<r?8xXBUS'1hY=i3OByn=HStv=y&_m&Zx5QhVx$8u2+R)mo}Z`F*@v!U'n||$fcBQi{bV0{^6^Z9cHy+DyOY+9gFyw#/5%~<g.kyzz*$:Q~q9(rO^{S")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "N")
                .cookie( "nonEmpty", "V$cCu*ZqWPsO:Af[:GF)_1ctcut_iJoH_Z)j$5(AK(acAMQKLBf3[]'kLss++uy/%175`6tNy[2TnL@|b+cw]h=)F1reH270/3<HA^I(:GIHQ)IPo*zK#_acf/&(7c9tsY4I_nrL*Q*s*l!2<pfbP+M}Gd.F+=jtY0~1Tm7^>cIEw=tCYbK:&bn<=w><RxZ70Vk=1oV()V(~P(Uy)")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "pnr0]f4XHTthGYC/UF8P2%!8f5P:4h%VWYTs:!m3F9=9=Kl'+/UZy6V5[%p&zxCb]XhOM'T*qgb:h9E6<trfZQZQrLiLI>$>F>g[@tNKGfC)p_`5DOv8pSU&S[h}V%R0R@}@lHszlD*{)kf'*>&VoO?=:/'t[v")
                .cookie( "nonEmpty", "WUk+xmv~(/e6:s8$F8^!|Do%}A2h(R4-lu>$QSd%5|[Z)I&&:'I3ZMsCvVEO6T+%(#5Pnyef6bKd=y@Q'7Ve3z'R>[M~ML61W/mbcGTyR9FD{*#N2goef6zEsXK}|<%kVU2='<0dSmIhN?(0p@'9P7=`z|^0@1-<i_$7F{0OJZMMm67%&|z7}RV]2+EL=`X`d/")
                .cookie( "empty", null)
            .when()
                .request( "GET", "/cookie/string")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "1.]wwBkYH3PebC!v*R:6Opz@aA^3^qRv<f~!$zgQ6`GA(!dfz/2@{IIR9Of6rIbTC[}#P1+N06!]Hglg^Q}j4@Xy<w&`yaA8g&F_aqbp`ytX=JKbgj{NZt?numfm#.B~7q5<x:|i7a)L*_Cb`EypbSXQ16`focVJTJG{q#LUR^_DLx]3D@pRz}vm~`++Lui}.!AT7_35~p`=nv[9[erCzh!nR9Z8Xo")
                .cookie( "empty", "")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "tVeEH~/~RhkZ_e~@")
                .cookie( "nonEmpty", null)
                .cookie( "empty", "")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_NonEmptyValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nullable", "_Qvh7.+B+_/lP:y)_c~[*np'@4)5e|)mP?S'GqMgTSz&[3Jc_+rcmYK|s:>N(~R5->zM]1K23S=mF.^=Qz#?`H7l*U8ty2vaN!FBysN!w|lDyk{*Ub@@7eRL(SPN_-OIyte#_)1KPBjvQ3}cM)J^NP=rDoG%$!Bp6MtNL^iv//66Or}lk?OR!T&@&T?R-v1l`j+=K/[zfl1")
                .cookie( "nonEmpty", "dv")
                .cookie( "empty", "")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                // nonEmpty.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getCookieString_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "nonEmpty", "w~/'Yp._ET_Jux?Ll#b0&Ci)X!#mc9Kz[n{<O7^Cm`x(giH/8}qzK4obWCa_6]]Y/BB2Z[x9:q0/Gx^8q]|s@[3mUM}[*r@KEzpBCS?A?O16ei-<&qx5V4NRrn6phu(bSWn7*U)Yrv3W")
                .cookie( "empty", "")
            .when()
                .request( "GET", "/cookie/string")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/cookie/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/cookie/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-946758145")
                .queryParam( "nonEmpty", "976538274")
                .queryParam( "nonEmpty", "-1049779932")
                .queryParam( "empty", "")
            .when()
                .request( "GET", "/query/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-409788165")
                .queryParam( "nullable", (String) null)
                .queryParam( "nonEmpty", (String) null)
                .queryParam( "nonEmpty", "244527022")
                .queryParam( "nonEmpty", "541605225")
                .queryParam( "nonEmpty", "-818255407")
                .queryParam( "nonEmpty", "-193544758")
                .queryParam( "nonEmpty", "-319090172")
                .queryParam( "nonEmpty", "483659607")
                .queryParam( "nonEmpty", "-173886350")
                .queryParam( "nonEmpty", "-432759842")
                .queryParam( "nonEmpty", "-840160859")
                .queryParam( "nonEmpty", "-276680525")
                .queryParam( "nonEmpty", "541605225")
                .queryParam( "nonEmpty", "-937206653")
                .queryParam( "nonEmpty", "-832672209")
                .queryParam( "nonEmpty", "630278341")
                .queryParam( "empty", "-568615503")
            .when()
                .request( "GET", "/query/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyItemsSize_Is_Gt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", (String) null)
                .queryParam( "exploded", "718109701")
                .queryParam( "nullable", "-69516156")
                .queryParam( "nonEmpty", "0")
                .queryParam( "nonEmpty", "-690938211")
                .queryParam( "nonEmpty", "-138887481")
                .queryParam( "empty", (String) null)
                .queryParam( "empty", "295321005")
                .queryParam( "empty", "-89076005")
                .queryParam( "empty", "800966307")
                .queryParam( "empty", "727858809")
                .queryParam( "empty", "-461638436")
                .queryParam( "empty", "-15830534")
                .queryParam( "empty", "-801618176")
                .queryParam( "empty", "-542755596")
                .queryParam( "empty", "930079291")
                .queryParam( "empty", "-615477119")
                .queryParam( "empty", "231473247")
                .queryParam( "empty", "-747779497")
                .queryParam( "empty", "-1043022183")
                .queryParam( "empty", "-370754972")
                .queryParam( "empty", "-1054827875")
            .when()
                .request( "GET", "/query/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "")
                .queryParam( "nullable", (String) null)
                .queryParam( "nullable", "183027205")
                .queryParam( "nonEmpty", "205978141")
                .queryParam( "nonEmpty", "891562829")
                .queryParam( "nonEmpty", "-311604734")
                .queryParam( "nonEmpty", "25004459")
                .queryParam( "nonEmpty", "440827284")
                .queryParam( "nonEmpty", "25004459")
                .queryParam( "nonEmpty", "549383527")
                .queryParam( "nonEmpty", "-837321265")
                .queryParam( "nonEmpty", "103026288")
                .queryParam( "nonEmpty", "-117342421")
                .queryParam( "empty", "0")
            .when()
                .request( "GET", "/query/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "0")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-374764058")
                .queryParam( "nonEmpty", "-947054118")
                .queryParam( "nonEmpty", "92548822")
                .queryParam( "empty", "1037224087")
                .queryParam( "empty", "653875233")
                .queryParam( "empty", "361362064")
                .queryParam( "empty", "-489511924")
                .queryParam( "empty", "397261534")
                .queryParam( "empty", "802321956")
                .queryParam( "empty", "-941843421")
                .queryParam( "empty", "-963073940")
                .queryParam( "empty", "256806840")
                .queryParam( "empty", "-153606395")
                .queryParam( "empty", "335508388")
                .queryParam( "empty", "249323391")
                .queryParam( "empty", "936666192")
                .queryParam( "empty", "653875233")
                .queryParam( "empty", "128242613")
                .queryParam( "empty", "-268960857")
            .when()
                .request( "GET", "/query/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NullableItemsContainsValue_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "715649797")
                .queryParam( "exploded", "792863197")
                .queryParam( "exploded", "-781505315")
                .queryParam( "exploded", "882457041")
                .queryParam( "exploded", "638363719")
                .queryParam( "exploded", "127682718")
                .queryParam( "exploded", "-146311030")
                .queryParam( "exploded", "967908622")
                .queryParam( "exploded", "-377188700")
                .queryParam( "exploded", "-822201812")
                .queryParam( "exploded", "967908622")
                .queryParam( "exploded", "78846419")
                .queryParam( "nullable", "0")
                .queryParam( "nonEmpty", "0")
                .queryParam( "nonEmpty", "136710766")
                .queryParam( "nonEmpty", "-924716132")
                .queryParam( "nonEmpty", "-260533712")
                .queryParam( "nonEmpty", "972013296")
                .queryParam( "nonEmpty", "136710766")
                .queryParam( "nonEmpty", "368833137")
                .queryParam( "nonEmpty", "-242621399")
                .queryParam( "nonEmpty", "-670425985")
                .queryParam( "nonEmpty", "-93002050")
                .queryParam( "empty", "")
            .when()
                .request( "GET", "/query/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NullableItemsContainsValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "")
                .queryParam( "nullable", "635246595")
                .queryParam( "nullable", "-423446425")
                .queryParam( "nullable", "-936801275")
                .queryParam( "nullable", "-175173611")
                .queryParam( "nullable", "-192573165")
                .queryParam( "nullable", "-950503940")
                .queryParam( "nullable", "-175173611")
                .queryParam( "nullable", "-365551108")
                .queryParam( "nullable", "927804334")
                .queryParam( "nonEmpty", "65041865")
                .queryParam( "nonEmpty", "-189516033")
                .queryParam( "nonEmpty", "690235927")
                .queryParam( "empty", "-158408562")
            .when()
                .request( "GET", "/query/array")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-72341519")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-884616211")
                .queryParam( "nonEmpty", "731429781")
                .queryParam( "nonEmpty", "864353008")
                .queryParam( "nonEmpty", "1018882507")
                .queryParam( "nonEmpty", "-674126482")
                .queryParam( "nonEmpty", "-1063310230")
                .queryParam( "nonEmpty", "1063709628")
                .queryParam( "nonEmpty", "669240002")
                .queryParam( "nonEmpty", "731429781")
                .queryParam( "nonEmpty", "742809700")
                .queryParam( "nonEmpty", "368273769")
                .queryParam( "nonEmpty", "-37708869")
                .queryParam( "nonEmpty", "-486985993")
            .when()
                .request( "GET", "/query/array")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-248446971")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-1058884371")
                .queryParam( "nonEmpty", "908974841")
                .queryParam( "nonEmpty", "802325839")
                .queryParam( "nonEmpty", "-154202023")
                .queryParam( "nonEmpty", "802325839")
                .queryParam( "empty", (String) null)
            .when()
                .request( "GET", "/query/array")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-211927472")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-75946360")
                .queryParam( "nonEmpty", "548205182")
                .queryParam( "nonEmpty", "-955614672")
                .queryParam( "nonEmpty", "223696895")
                .queryParam( "nonEmpty", "357908209")
                .queryParam( "nonEmpty", "553809817")
                .queryParam( "nonEmpty", "1007681959")
                .queryParam( "nonEmpty", "-937381090")
                .queryParam( "nonEmpty", "-652828067")
                .queryParam( "nonEmpty", "-44533766")
                .queryParam( "nonEmpty", "-72568761")
                .queryParam( "nonEmpty", "-955614672")
                .queryParam( "nonEmpty", "-673522965")
                .queryParam( "nonEmpty", "195851936")
                .queryParam( "nonEmpty", "-882105637")
                .queryParam( "gvrtbmskmnelqpv", "-545.1")
                .queryParam( "fxkj", "-139")
            .when()
                .request( "GET", "/query/array")
            .then()
                // empty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_EmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-826993243")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-585470707")
                .queryParam( "nonEmpty", "-950227427")
                .queryParam( "nonEmpty", "721082979")
                .queryParam( "nonEmpty", "-105377140")
                .queryParam( "nonEmpty", "-101330824")
                .queryParam( "nonEmpty", "721082979")
                .queryParam( "empty", "true")
                .queryParam( "empty", "-706935601")
                .queryParam( "empty", "57572432")
                .queryParam( "empty", "-182815524")
                .queryParam( "empty", "280400531")
                .queryParam( "empty", "1034647486")
                .queryParam( "empty", "-143894549")
                .queryParam( "empty", "-143397813")
                .queryParam( "empty", "432835356")
                .queryParam( "empty", "-770854029")
            .when()
                .request( "GET", "/query/array")
            .then()
                // empty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-586007710")
                .queryParam( "nullable", "")
                .queryParam( "empty", "0")
                .queryParam( "empty", "502985810")
                .queryParam( "empty", "432246686")
                .queryParam( "empty", "775003968")
                .queryParam( "empty", "889044240")
                .queryParam( "empty", "44075002")
                .queryParam( "empty", "-1065000929")
                .queryParam( "empty", "-895277541")
                .queryParam( "empty", "9116951")
                .queryParam( "empty", "316441604")
                .queryParam( "empty", "-131013535")
                .queryParam( "empty", "-362061343")
                .queryParam( "empty", "-68168479")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-417805009")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", (String) null)
                .queryParam( "empty", "0")
                .queryParam( "empty", "528309323")
                .queryParam( "empty", "951994780")
                .queryParam( "empty", "-907519541")
                .queryParam( "empty", "-246655240")
                .queryParam( "empty", "366998639")
                .queryParam( "empty", "-934943784")
                .queryParam( "empty", "-71154022")
                .queryParam( "empty", "683050683")
                .queryParam( "empty", "-419806350")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NonEmptyType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-978724008")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "true")
                .queryParam( "empty", "0")
                .queryParam( "empty", "127687259")
                .queryParam( "empty", "-474172041")
                .queryParam( "empty", "-1061435334")
                .queryParam( "empty", "529194130")
                .queryParam( "empty", "320654082")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nonEmpty.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NonEmptyItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-860553158")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-540383109")
                .queryParam( "nonEmpty", "-540383109")
                .queryParam( "empty", "0")
                .queryParam( "empty", "775662270")
                .queryParam( "empty", "476706974")
                .queryParam( "empty", "-357350020")
                .queryParam( "empty", "918750389")
                .queryParam( "empty", "112261482")
                .queryParam( "empty", "432312744")
                .queryParam( "empty", "607916881")
                .queryParam( "empty", "1056346689")
                .queryParam( "empty", "-131859165")
                .queryParam( "empty", "-517030818")
                .queryParam( "empty", "743536357")
                .queryParam( "empty", "56967725")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nonEmpty.Items.Size=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NonEmptyItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-460973943")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-469.0")
                .queryParam( "nonEmpty", "966003976")
                .queryParam( "nonEmpty", "1008209210")
                .queryParam( "nonEmpty", "1047013775")
                .queryParam( "nonEmpty", "846383606")
                .queryParam( "nonEmpty", "1008209210")
                .queryParam( "nonEmpty", "573395544")
                .queryParam( "empty", "0")
                .queryParam( "empty", "528079217")
                .queryParam( "empty", "-764209970")
                .queryParam( "empty", "-886359421")
                .queryParam( "empty", "-635686961")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nonEmpty.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-263864068")
                .queryParam( "nonEmpty", "-384662145")
                .queryParam( "nonEmpty", "74773304")
                .queryParam( "nonEmpty", "-971422379")
                .queryParam( "nonEmpty", "-651569829")
                .queryParam( "nonEmpty", "-971422379")
                .queryParam( "nonEmpty", "-439283806")
                .queryParam( "nonEmpty", "627219729")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-41360311")
                .queryParam( "empty", "-859872662")
                .queryParam( "empty", "1046357294")
                .queryParam( "empty", "616308450")
                .queryParam( "empty", "-483488510")
                .queryParam( "empty", "-1071156057")
                .queryParam( "empty", "812313234")
                .queryParam( "empty", "-790310697")
                .queryParam( "empty", "-244454568")
                .queryParam( "empty", "-178778316")
                .queryParam( "empty", "-446624343")
                .queryParam( "empty", "716808335")
                .queryParam( "empty", "-1015034718")
                .queryParam( "empty", "-922194353")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NullableType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-592319427")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-559960964")
                .queryParam( "nonEmpty", "753086654")
                .queryParam( "nonEmpty", "501854320")
                .queryParam( "nonEmpty", "901453155")
                .queryParam( "nonEmpty", "-763430474")
                .queryParam( "nonEmpty", "-647330132")
                .queryParam( "nonEmpty", "-873720767")
                .queryParam( "nonEmpty", "507305559")
                .queryParam( "nonEmpty", "68522676")
                .queryParam( "nonEmpty", "-785967917")
                .queryParam( "nonEmpty", "407064787")
                .queryParam( "nonEmpty", "-873720767")
                .queryParam( "nonEmpty", "-483581005")
                .queryParam( "nonEmpty", "960626317")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-991865915")
                .queryParam( "empty", "748707165")
                .queryParam( "empty", "423831805")
                .queryParam( "empty", "43589412")
                .queryParam( "empty", "316717217")
                .queryParam( "empty", "873073606")
                .queryParam( "empty", "1025282594")
                .queryParam( "empty", "870300122")
                .queryParam( "empty", "-35154339")
                .queryParam( "empty", "-1042737142")
                .queryParam( "empty", "-287101809")
                .queryParam( "empty", "515363439")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nullable.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_NullableItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "-333953228")
                .queryParam( "nullable", "332.6")
                .queryParam( "nonEmpty", "-631342977")
                .queryParam( "nonEmpty", "696911911")
                .queryParam( "nonEmpty", "368659127")
                .queryParam( "nonEmpty", "-747842113")
                .queryParam( "nonEmpty", "877640584")
                .queryParam( "nonEmpty", "987236880")
                .queryParam( "nonEmpty", "696911911")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-493020903")
                .queryParam( "empty", "365452594")
                .queryParam( "empty", "-158015188")
                .queryParam( "empty", "-691679221")
                .queryParam( "empty", "-988586485")
                .queryParam( "empty", "-549008269")
                .queryParam( "empty", "-542867155")
                .queryParam( "empty", "-831807049")
                .queryParam( "empty", "-792913942")
                .queryParam( "empty", "-985128916")
                .queryParam( "empty", "-723209788")
                .queryParam( "empty", "-290390595")
                .queryParam( "empty", "-19283938")
                .queryParam( "empty", "455459495")
                .queryParam( "empty", "847647298")
            .when()
                .request( "GET", "/query/array")
            .then()
                // nullable.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-395077637")
                .queryParam( "nonEmpty", "790982465")
                .queryParam( "nonEmpty", "-275745314")
                .queryParam( "nonEmpty", "-699968594")
                .queryParam( "nonEmpty", "-954460197")
                .queryParam( "nonEmpty", "-562176900")
                .queryParam( "nonEmpty", "960723292")
                .queryParam( "nonEmpty", "115819170")
                .queryParam( "nonEmpty", "488764887")
                .queryParam( "nonEmpty", "-275745314")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-701386008")
                .queryParam( "empty", "-74529999")
                .queryParam( "empty", "140594522")
                .queryParam( "empty", "243046145")
                .queryParam( "empty", "-406467783")
            .when()
                .request( "GET", "/query/array")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", (String) null)
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-831781577")
                .queryParam( "nonEmpty", "1022749291")
                .queryParam( "nonEmpty", "-657292686")
                .queryParam( "nonEmpty", "235064740")
                .queryParam( "nonEmpty", "523460091")
                .queryParam( "nonEmpty", "523460091")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-443126775")
            .when()
                .request( "GET", "/query/array")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_ExplodedType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "`0Q;@f")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-376423109")
                .queryParam( "nonEmpty", "861099256")
                .queryParam( "nonEmpty", "979808873")
                .queryParam( "nonEmpty", "496020430")
                .queryParam( "nonEmpty", "678295333")
                .queryParam( "nonEmpty", "385645891")
                .queryParam( "nonEmpty", "-58248360")
                .queryParam( "nonEmpty", "1043257301")
                .queryParam( "nonEmpty", "-166964939")
                .queryParam( "nonEmpty", "409541129")
                .queryParam( "nonEmpty", "-688821438")
                .queryParam( "nonEmpty", "-38837272")
                .queryParam( "nonEmpty", "-343054362")
                .queryParam( "nonEmpty", "-343054362")
                .queryParam( "nonEmpty", "-164920784")
                .queryParam( "nonEmpty", "672760132")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-572663917")
                .queryParam( "empty", "-723867078")
                .queryParam( "empty", "-910997875")
                .queryParam( "empty", "-51485905")
                .queryParam( "empty", "-530929470")
            .when()
                .request( "GET", "/query/array")
            .then()
                // exploded.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryArray_ExplodedItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "exploded", "pik,-162.1,xibveufemifawrll,wB")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-452680549")
                .queryParam( "nonEmpty", "-111445797")
                .queryParam( "nonEmpty", "-282720798")
                .queryParam( "nonEmpty", "962555649")
                .queryParam( "nonEmpty", "-123731175")
                .queryParam( "nonEmpty", "-803554183")
                .queryParam( "nonEmpty", "550846493")
                .queryParam( "nonEmpty", "-111445797")
                .queryParam( "empty", "0")
                .queryParam( "empty", "173760309")
                .queryParam( "empty", "79786887")
                .queryParam( "empty", "769034884")
                .queryParam( "empty", "-389240300")
                .queryParam( "empty", "375880103")
                .queryParam( "empty", "-399027577")
                .queryParam( "empty", "-558516118")
                .queryParam( "empty", "624177041")
                .queryParam( "empty", "-759910376")
                .queryParam( "empty", "648756300")
                .queryParam( "empty", "-669106153")
                .queryParam( "empty", "-129272108")
                .queryParam( "empty", "484946493")
                .queryParam( "empty", "-524361356")
            .when()
                .request( "GET", "/query/array")
            .then()
                // exploded.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/array", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/array", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[width]", "0")
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[agecvztdevmustb]", "true")
                .queryParam( "deep[nbnwcl]", "(Z")
                .queryParam( "width", "0")
                .queryParam( "height", "0")
                .queryParam( "alkze", "o}")
                .queryParam( "cjdyvcrvmflakqdn", "-1006.8")
                .queryParam( "swgtdnruwoxpvxmv", "true")
                .queryParam( "nullable", "")
                .queryParam( "width", "0")
                .queryParam( "height", "0")
                .queryParam( "kkomz", "}g7N@:0")
                .queryParam( "yqiyhywy", "hkiawxwxo,-625,aeibviwumdxivpi,-10")
                .queryParam( "nhnnig", "728")
            .when()
                .request( "GET", "/query/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[width]", (String) null)
                .queryParam( "width", (String) null)
                .queryParam( "nullable", (String) null)
                .queryParam( "height", (String) null)
            .when()
                .request( "GET", "/query/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", (String) null)
                .queryParam( "deep[nomrlexvbvfsr]", "863")
                .queryParam( "deep[vibggtrmwcqhohe]", ", _qpTW],v^=")
                .queryParam( "height", (String) null)
                .queryParam( "gkihoxjtn", ",fs\\")
                .queryParam( "lrmmpkbvvo", "sjwswhvxxz,395")
                .queryParam( "nullable", "width 0 height 0 gbdzvyp true qjfriluglgqxtev k5%FYFe")
                .queryParam( "width", (String) null)
                .queryParam( "tgspmbvmospds", "true")
                .queryParam( "gxz", ",FZ;,jRz")
            .when()
                .request( "GET", "/query/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[width]", "111626826")
                .queryParam( "width", "384103148")
                .queryParam( "nullable", "width ")
                .queryParam( "width", "868468626")
                .queryParam( "height", "199557484")
            .when()
                .request( "GET", "/query/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableValuePropertiesWidthValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "1038458536")
                .queryParam( "deep[krmrymlflupfcz]", "le")
                .queryParam( "deep[xkmkugkmycn]", "c")
                .queryParam( "deep[regxwxduzedqjfq]", "")
                .queryParam( "height", "231524417")
                .queryParam( "jmqlmatldlbgn", "")
                .queryParam( "fzeehbhmjwfdqah", "ljsbkwqz,262.1,bptfpytchojoop,t++\".")
                .queryParam( "sudvyjsx", "640.8")
                .queryParam( "nullable", "width 274584294 height  ulvrjpyoxebkx true ogv 998 n 945.5")
                .queryParam( "xibip", "xbdrocdtzpq,om*\\V,v/&5,gnszeulyyjloqdsw,zw,E)Ss")
                .queryParam( "hvfc", "-161")
                .queryParam( "ckpollwcznoamo", "")
            .when()
                .request( "GET", "/query/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableValuePropertiesHeightValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[width]", "0")
                .queryParam( "width", "0")
                .queryParam( "nullable", "height 207601165")
                .queryParam( "width", "0")
                .queryParam( "height", "0")
            .when()
                .request( "GET", "/query/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[vsbkmwt]", "<1v[4&1")
                .queryParam( "height", "0")
                .queryParam( "eimgdokpxsjxige", "/c,PS{u{j>X")
                .queryParam( "kvljgvoab", "%")
                .queryParam( "nullable", "width 0 uvou bspwdlfzd,762.6,znxnypvm,668.6,capwftptbcwghyhz,l-s6Ob lzcginjmhkjzjdd 150.8 rqkqpotnrfnmbvwj 495.5")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[zfcqisrpcpqsnyn]", "")
                .queryParam( "height", "0")
                .queryParam( "eii", "true")
                .queryParam( "ptihfxlbpgpjzgi", "0gNzVEB")
                .queryParam( "sirshfpjnmlgkc", ".#'Ql*w',x)7 i,Fs/fI")
                .queryParam( "nullable", "width 0 wrbgtzsk -111.5")
                .queryParam( "nonEmpty", (String) null)
            .when()
                .request( "GET", "/query/object")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[mbpa]", "-75")
                .queryParam( "deep[ocxlsydelqzdig]", "")
                .queryParam( "deep[efpctii]", "-")
                .queryParam( "height", "0")
                .queryParam( "nebumaaihxpxoqem", "_}OtD^*b,m")
                .queryParam( "dmuiwm", "gJ1")
                .queryParam( "mvsmvrqjnvmrb", "}'OT7c(")
                .queryParam( "nullable", "width 0 jegpbzi 5K4~lo,l.")
                .queryParam( "nonEmpty", "true")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nonEmpty.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[uiazicdk]", "")
                .queryParam( "deep[o]", "t5y_bI&,:q1a ],kTwZ")
                .queryParam( "height", "0")
                .queryParam( "yyls", "")
                .queryParam( "d", "true")
                .queryParam( "nullable", "width 0 tcygp 561.5")
                .queryParam( "width", "true")
                .queryParam( "ckxrajwguwoyfqfo", "2AoP<,&V'tL")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nonEmpty.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[jvuncpjnqz]", "_jI")
                .queryParam( "deep[ei]", ",j]hjD7S")
                .queryParam( "deep[pqgdompeyvnwnywh]", "true")
                .queryParam( "height", "0")
                .queryParam( "mrfxoaz", "357.3")
                .queryParam( "qamxkhyeblmbs", "wtimts,:L.WR,M U[)6t,]a]_`hvb,mtqnuzuevmpndve,V /n\"LB0,bskvujyscfvgrmvd,")
                .queryParam( "nullable", "width 0 wyjecqid 15Co,")
                .queryParam( "width", "-1")
                .queryParam( "nywozlfqgr", "rqbzhothcunnjf,,atbvtinkh,-570")
                .queryParam( "qvwpshfzuqj", "true")
                .queryParam( "k", "941")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nonEmpty.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[dgdgcqzi]", "")
                .queryParam( "height", "0")
                .queryParam( "fnjmkyyitv", "-889")
                .queryParam( "stte", "-987.3")
                .queryParam( "nullable", "width 0 cpxmcqlalh ax pxterk -867.3 tihfynosn 333.7")
                .queryParam( "height", "zeecnia,-103.6,hezb,v$S(P\\Q~,ufu,766")
                .queryParam( "vsgzwwwycy", "-24.9")
                .queryParam( "gr", "$URw,$ ")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nonEmpty.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NonEmptyValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[jslbecjhjroyjjm]", "gvjeaealwljabcq,305,kiypom,-849,dhfzndymjwkdauoh,-997.6")
                .queryParam( "deep[dvlu]", "b")
                .queryParam( "height", "0")
                .queryParam( "tkwiibzemdw", "I")
                .queryParam( "buzqrknt", "-298")
                .queryParam( "nullable", "width 0 urehasxdztml 940 itfmpnvot true")
                .queryParam( "height", "-1")
                .queryParam( "zssyjrpdztpo", "true")
                .queryParam( "zuy", "wnhwjvqg,-179,vzzmjxpsdij,SOFxIfr,qozasjmvinhptig,:uH$clo")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nonEmpty.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[bgk]", "gqjoafbtfd,xMR%(]")
                .queryParam( "deep[qxpktkrkfjrixl]", "-607")
                .queryParam( "deep[ccwcjvsmbnkcd]", "-156.2")
                .queryParam( "height", "0")
                .queryParam( "fu", "|**b6")
                .queryParam( "kxjzwvmdik", "279")
                .queryParam( "czbsedgvgshbu", "true")
                .queryParam( "ycswm", "937.5")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[mp]", "true")
                .queryParam( "deep[ckqqevpqphgkdyru]", "true")
                .queryParam( "height", "0")
                .queryParam( "eucjmben", "")
                .queryParam( "zzmijdktvduee", "*\\")
                .queryParam( "nullable", "431.2")
                .queryParam( "rvqcrkz", "")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nullable.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[mmhwbue]", "q4o7D")
                .queryParam( "height", "0")
                .queryParam( "jtulktscabprcvm", "nvxvpxltqnis,923.1,tdkrvoiecrhwxb,-807,bw,")
                .queryParam( "tzlbymwhlvc", "6i'o$1Jr")
                .queryParam( "nullable", "width -499.6 gnvjwfd -227")
                .queryParam( "zapwqixhx", "XkY")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nullable.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[wywdfsgspmv]", "-1023")
                .queryParam( "deep[agyuipfjtqdlqfz]", "true")
                .queryParam( "height", "0")
                .queryParam( "kthjfwygeakyd", "750")
                .queryParam( "wfwmeegmmbiola", "936.8")
                .queryParam( "wikuvkqmvptyyk", "true")
                .queryParam( "nullable", "width -1 dew gnvqrwlq,+czUHk,5}`YX,m&\"L,r,,}v[):,#\"4e,ng,true")
                .queryParam( "coojofunpaoov", "0VVt+,{/hLP,G")
                .queryParam( "of", "-481")
                .queryParam( "nzdpatwvvudnimu", "-732.7")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nullable.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[csx]", "cksqz,bl,qkb,-841.5,qftocwkpmcocxpo,-855")
                .queryParam( "height", "0")
                .queryParam( "nbtjy", "310")
                .queryParam( "nullable", "width 0 height .YsCSvZ( tlmxtldifqh -769.7 ud *,-Or,OW9XYo#")
                .queryParam( "ziapexkr", "913")
                .queryParam( "y", "-450.7")
                .queryParam( "zakpuangtzyr", "503")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nullable.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_NullableValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[nxvz]", "^Lk")
                .queryParam( "height", "0")
                .queryParam( "pzbuofpjkexcxnr", "431.5")
                .queryParam( "r", "l8TB#J,UY]%+,vtHp$F")
                .queryParam( "nullable", "width 0 height -1 weyndo -624 ixgsbegvlrxf -747")
                .queryParam( "ldlzgtljal", "")
                .queryParam( "pdylsjbvheeftpxt", "F0XeE ,#!,/Ak<[~")
                .queryParam( "njnhznnflqok", "true")
            .when()
                .request( "GET", "/query/object")
            .then()
                // nullable.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_ExplodedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[ygbrlgrtn]", "")
                .queryParam( "deep[qdblvayphy]", "rz3[q|B")
                .queryParam( "nullable", "width 0 bnr e,680.1,fzhwbnfjnidb,-247,hhfz,true")
                .queryParam( "fenkulrtb", "nj,,yh,ggkkodphp,{q,ezbpybiwoetlbnpf,-784.2")
                .queryParam( "mn", "46.3")
                .queryParam( "tqnuxihciy", "qdjdcgrl,-634.4,grpcynzhkzvzz,756,pjuzbdnzhn,true")
            .when()
                .request( "GET", "/query/object")
            .then()
                // exploded.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_ExplodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[hdbyaugqyhts]", "true")
                .queryParam( "deep[hpxlkoilytl]", "353")
                .queryParam( "exploded", (String) null)
                .queryParam( "nullable", "width 0 ykrtzpujgssfmzx UaXT*+7&,kJp\\] bklmgmzdtmwwt <s?)O][v")
                .queryParam( "cclhvnrzighsjzv", "")
            .when()
                .request( "GET", "/query/object")
            .then()
                // exploded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_ExplodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[pekalpfph]", "2[g")
                .queryParam( "exploded", "-978")
                .queryParam( "nullable", "width 0 dfbfgwjudcwc 1IhHZn,[{Ai*%O~")
                .queryParam( "wlcckvpbcq", "-366")
            .when()
                .request( "GET", "/query/object")
            .then()
                // exploded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[xnvh]", "55")
                .queryParam( "width", "nwgrywfzriwnt,-590.6,bojirukpcbixcwc,^b,xuznltyorxuwpm,true")
                .queryParam( "height", "0")
                .queryParam( "jjm", "-154")
                .queryParam( "jwwoysq", "1016")
                .queryParam( "nullable", "width 0 tmghz $Ewof:cS,YiI}oB>,j")
                .queryParam( "hyrkywglyik", "true")
            .when()
                .request( "GET", "/query/object")
            .then()
                // exploded.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[sucsxjatsvqxp]", "")
                .queryParam( "deep[s]", "337")
                .queryParam( "width", "-1")
                .queryParam( "height", "0")
                .queryParam( "wgzkw", "")
                .queryParam( "nullable", "width 0 socgicjgntfab ujzijrasjldjobnz,true oaijfhhat 551.3 gveyrfwfyeqi 14")
                .queryParam( "kg", "rGg\"9o4")
                .queryParam( "n", "${")
                .queryParam( "zclfosyj", "")
            .when()
                .request( "GET", "/query/object")
            .then()
                // exploded.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[xjyfv]", "xp&udXG")
                .queryParam( "deep[djufrgnpywoq]", "u>uDn,fBk{cKcL,r")
                .queryParam( "deep[rg]", "}`Cw")
                .queryParam( "height", "true")
                .queryParam( "hxwfy", "")
                .queryParam( "qfqfhmcxbxzod", "783.8")
                .queryParam( "nullable", "width 0 y etiojmkkokc,948 fkhcfzpxyxisupfw true")
                .queryParam( "nfknvtpfxnhh", "true")
                .queryParam( "uxyqhoxrz", "")
            .when()
                .request( "GET", "/query/object")
            .then()
                // exploded.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_ExplodedValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[kxyjk]", "true")
                .queryParam( "deep[qcmcyibbjfsg]", ";JI=jmuT")
                .queryParam( "deep[erfxitoh]", "")
                .queryParam( "height", "-1")
                .queryParam( "ndvkqduxkse", "181.0")
                .queryParam( "cwzhls", "-726")
                .queryParam( "lqwvb", "gasindc,true,jxkdhapqbhiaiiw,true,fqvwobdcgr,true")
                .queryParam( "nullable", "width 0 mgupphi 296.4 bdjnbsqkjuw 66")
                .queryParam( "ryncabhgwte", "`h7s2^")
                .queryParam( "fkytx", "i<#-`^9")
            .when()
                .request( "GET", "/query/object")
            .then()
                // exploded.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_DeepDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "height", "0")
                .queryParam( "nnrhyuybo", "-682.0")
                .queryParam( "wol", "")
                .queryParam( "nullable", "width 0 x 827")
                .queryParam( "nvojyjsuz", "true")
                .queryParam( "txitxwiwhqfbj", "2?WSw8t")
            .when()
                .request( "GET", "/query/object")
            .then()
                // deep.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_DeepType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep", (String) null)
                .queryParam( "height", "0")
                .queryParam( "bdbl", "674.6")
                .queryParam( "mdzbzrtctmargmb", "-933")
                .queryParam( "nullable", "width 0 vcjcvltnr 151")
                .queryParam( "xhieoahttn", "true")
            .when()
                .request( "GET", "/query/object")
            .then()
                // deep.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_DeepType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep", ",3@X|\\")
                .queryParam( "height", "0")
                .queryParam( "qtwhgb", ")k#bI%")
                .queryParam( "nullable", "width 0 pr &Zt isjuraet 579 ukjbhztczkygjjjy eitkiblomjgxnik,")
                .queryParam( "dierwxvksa", "ygxw,true,zlyjkzo,vEC")
            .when()
                .request( "GET", "/query/object")
            .then()
                // deep.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_DeepValuePropertiesWidthType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[width]", "true")
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[tbawgnkdopqfvkxh]", "c,<,txyptdirjj,850,pisudauruo,13.0")
                .queryParam( "height", "0")
                .queryParam( "erkfxmeignimstp", "-66.2")
                .queryParam( "d", "")
                .queryParam( "nullable", "width 0 mccea  ntkpjud -265.0 prinyxtmia true")
                .queryParam( "femgzvg", "RjuU6&")
                .queryParam( "bhbvticwb", "820")
                .queryParam( "qkfpptiwgbjaigpe", "true")
            .when()
                .request( "GET", "/query/object")
            .then()
                // deep.Value.Properties.width.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_DeepValuePropertiesWidthValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[width]", "-1")
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[jzbvg]", "906.2")
                .queryParam( "height", "0")
                .queryParam( "ybojgjpwnutqihpn", "")
                .queryParam( "nullable", "width 0 pxiuho -933")
                .queryParam( "lsgqjvm", "535")
            .when()
                .request( "GET", "/query/object")
            .then()
                // deep.Value.Properties.width.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_DeepValuePropertiesHeightType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "(B")
                .queryParam( "deep[tbphyicpazagr]", "true")
                .queryParam( "deep[iklwqygq]", "-823")
                .queryParam( "deep[zmxmlbkoyvzb]", "wbkmpdsrxl,true,hjurx,186")
                .queryParam( "height", "0")
                .queryParam( "x", "|khQ/A")
                .queryParam( "qxpazcctzci", "mcihlbcqlmv,-31.0")
                .queryParam( "nullable", "width 0 sizir qzkrvirbfis,Fq#P-(,nchcz,625.5,hrhpkumyiyghya,-771.9 bhdjqlilofpc |")
                .queryParam( "qtwittvdymvw", "132")
                .queryParam( "jbduvn", "")
                .queryParam( "mw", "true")
            .when()
                .request( "GET", "/query/object")
            .then()
                // deep.Value.Properties.height.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryObject_DeepValuePropertiesHeightValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "deep[height]", "-1")
                .queryParam( "deep[vd]", "qtekm,51,ojhnwv,true,wbvrrfyjgtvili,-156.8")
                .queryParam( "deep[ljkayyivqlxylftq]", "")
                .queryParam( "deep[ellerzxmc]", "-655.0")
                .queryParam( "height", "0")
                .queryParam( "zxoigxgzocwqcm", "421")
                .queryParam( "iiqwqkji", "56")
                .queryParam( "oco", "")
                .queryParam( "nullable", "width 0 m true")
                .queryParam( "gwezrmvbb", "693")
                .queryParam( "ajhmxndign", "-264")
                .queryParam( "xgrxdgyqyveokzv", "rhhhclbpp,-316")
            .when()
                .request( "GET", "/query/object")
            .then()
                // deep.Value.Properties.height.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_EmptyDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", "S0)$yhhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wz")
                .queryParam( "nonEmpty", "x6{")
                .queryParam( "empty", "h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=x\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*a")
            .when()
                .request( "GET", "/query/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_EmptyValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", (String) null)
                .queryParam( "nonEmpty", ")@b&1avN2IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^)1=$}o%zg")
                .queryParam( "empty", "")
            .when()
                .request( "GET", "/query/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_NullableValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "\\bD")
                .queryParam( "empty", "dep5FtnA\":SD4:uw,pGY`UWA9GqrV'Au!L.sq*Pf%z&b?lK{2hFEDwG[AcSVW%M'|y3{f5xCL8-Cu|k*Ia9A+*A,|L?-G=V]L8CYg t<P]#{k%AlU1l`p?H|Q~4%@~[NgZ> 9M3>cv.$m]f7qH[r3.GC#rU f7R#?y[aaJ#Y:%`$s(A{@\\@s<\"M5rH[vC?>7(^zKpUPb{ CO2qR}\\Dk iFXrX\"XH&hN'7dl-FE7F$i?^G*Q\\N")
            .when()
                .request( "GET", "/query/string")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_EmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", "x*:/3GOUKk5L\"y^5as@3O-;]90Q2kNWES$tna")
                .queryParam( "nonEmpty", "bbEvzR;]Y#'(0I")
            .when()
                .request( "GET", "/query/string")
            .then()
                // empty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_EmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", "{Lr>ddf523Cj{(#ur5@!]i@")
                .queryParam( "nonEmpty", "Nx2T>[ T.n+4e/QXQFoMTvrLW#X}")
                .queryParam( "empty", (String) null)
            .when()
                .request( "GET", "/query/string")
            .then()
                // empty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_NonEmptyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", "P{C_2`OKs914cJYO&=(T>{hfh'##I/*uigZ6GmM&Pm%` %DKf!jA,:\\(DkCa \"+#%nm,B5L3@': xQ`YeX<nq9zrZ\\>W/i'FFp3[cDuks&|t5vCKQjPAF_@KB&HOD7y]8s#C'wGb{A1k?w(RX{T1Jrw#^pfc'V[nyu0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwXgIZjb%WWDG:>2AvL#|L1:8E;u#>Dmy:o27HkDZ%rt)list")
                .queryParam( "empty", "")
            .when()
                .request( "GET", "/query/string")
            .then()
                // nonEmpty.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_NonEmptyType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", ">?]sAviyah`46\"yhpb\"~'tm1ZV>& S{DOg6(3Wtz>n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg],I=BTHC6;vgsi}{1YsSiTi|>S:c`%\";D'[b&IY0S#=[fBQR|Yk_")
                .queryParam( "nonEmpty", (String) null)
                .queryParam( "empty", "")
            .when()
                .request( "GET", "/query/string")
            .then()
                // nonEmpty.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_NonEmptyValueLength_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nullable", "V|=")
                .queryParam( "nonEmpty", "KN")
                .queryParam( "empty", "")
            .when()
                .request( "GET", "/query/string")
            .then()
                // nonEmpty.Value.Length=2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getQueryString_NullableDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "nonEmpty", "ip@[:+&rEEMcN.PyrD\\.9M5:</v2\\t|3I;&@@K~$JU@ihst_E[pP)^oQ)x4U1!` `FGJ+1Z~3\"P(Ys|!DI,F) >*$t{egzYY<JB](%E4N9BRTWHzX(0o|HhJW4M.w(:\"QB6?Nu^7M%pKSwr/BFl6m7V$v2vn mLD")
                .queryParam( "empty", "")
            .when()
                .request( "GET", "/query/string")
            .then()
                // nullable.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/query/string", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/query/string", response.statusCode(), responseHeaders( response));
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
