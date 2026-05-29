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
                .cookie( "nonEmpty", "-942214884")
                .cookie( "nonEmpty", "794263638")
                .cookie( "nonEmpty", "-625089485")
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
                .cookie( "exploded", "-558379580")
                .cookie( "nullable", null)
                .cookie( "nonEmpty", null)
                .cookie( "nonEmpty", "944118134")
                .cookie( "nonEmpty", "944118134")
                .cookie( "nonEmpty", "747940871")
                .cookie( "nonEmpty", "-878400812")
                .cookie( "nonEmpty", "-164167572")
                .cookie( "nonEmpty", "50720548")
                .cookie( "nonEmpty", "-315206283")
                .cookie( "nonEmpty", "-377199995")
                .cookie( "empty", "-187132777")
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
                .cookie( "exploded", "141808539")
                .cookie( "exploded", "870925827")
                .cookie( "exploded", "-687543513")
                .cookie( "exploded", "726954252")
                .cookie( "exploded", "608560821")
                .cookie( "exploded", "97990732")
                .cookie( "exploded", "1023568850")
                .cookie( "exploded", "-169502981")
                .cookie( "exploded", "659335640")
                .cookie( "nullable", "-614155757")
                .cookie( "nonEmpty", "0")
                .cookie( "nonEmpty", "-955828850")
                .cookie( "nonEmpty", "-394423881")
                .cookie( "empty", null)
                .cookie( "empty", "303459860")
                .cookie( "empty", "-423621996")
                .cookie( "empty", "-420270252")
                .cookie( "empty", "-395964257")
                .cookie( "empty", "-1021940647")
                .cookie( "empty", "-598729476")
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
                .cookie( "nullable", "-90796265")
                .cookie( "nullable", "750375712")
                .cookie( "nullable", "817023733")
                .cookie( "nullable", "486213579")
                .cookie( "nonEmpty", "972768219")
                .cookie( "nonEmpty", "-351624035")
                .cookie( "nonEmpty", "1034237512")
                .cookie( "nonEmpty", "286069445")
                .cookie( "nonEmpty", "-531042736")
                .cookie( "nonEmpty", "-981158525")
                .cookie( "nonEmpty", "519968394")
                .cookie( "nonEmpty", "113126640")
                .cookie( "nonEmpty", "-108205088")
                .cookie( "nonEmpty", "-542331978")
                .cookie( "nonEmpty", "-265759773")
                .cookie( "nonEmpty", "-283312652")
                .cookie( "nonEmpty", "52454")
                .cookie( "nonEmpty", "519968394")
                .cookie( "nonEmpty", "372351116")
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
                .cookie( "nonEmpty", "-53963032")
                .cookie( "nonEmpty", "-945811574")
                .cookie( "nonEmpty", "448646941")
                .cookie( "empty", "96500558")
                .cookie( "empty", "404989521")
                .cookie( "empty", "-746152259")
                .cookie( "empty", "936172903")
                .cookie( "empty", "-1049435800")
                .cookie( "empty", "-491872635")
                .cookie( "empty", "936172903")
                .cookie( "empty", "-1006129486")
                .cookie( "empty", "99358539")
                .cookie( "empty", "-803248217")
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
                .cookie( "exploded", "943411693")
                .cookie( "exploded", "867092044")
                .cookie( "exploded", "867092044")
                .cookie( "exploded", "345280893")
                .cookie( "exploded", "298832938")
                .cookie( "exploded", "-748305196")
                .cookie( "nullable", "0")
                .cookie( "nonEmpty", "0")
                .cookie( "nonEmpty", "-937071595")
                .cookie( "nonEmpty", "-664244294")
                .cookie( "nonEmpty", "-645252716")
                .cookie( "nonEmpty", "574803620")
                .cookie( "nonEmpty", "302734544")
                .cookie( "nonEmpty", "-965594052")
                .cookie( "nonEmpty", "625633873")
                .cookie( "nonEmpty", "-1040961792")
                .cookie( "nonEmpty", "-705960486")
                .cookie( "nonEmpty", "340497162")
                .cookie( "nonEmpty", "480027965")
                .cookie( "nonEmpty", "-645252716")
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
                .cookie( "nullable", "969578553")
                .cookie( "nullable", "65077350")
                .cookie( "nullable", "969578553")
                .cookie( "nonEmpty", "120182761")
                .cookie( "nonEmpty", "-411812864")
                .cookie( "nonEmpty", "-285163498")
                .cookie( "empty", "-697213579")
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
                .cookie( "exploded", "-414610962")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-839122087")
                .cookie( "nonEmpty", "-418337013")
                .cookie( "nonEmpty", "461087804")
                .cookie( "nonEmpty", "-534316081")
                .cookie( "nonEmpty", "-660174330")
                .cookie( "nonEmpty", "78059178")
                .cookie( "nonEmpty", "-171748020")
                .cookie( "nonEmpty", "-131579154")
                .cookie( "nonEmpty", "-660359612")
                .cookie( "nonEmpty", "-660174330")
                .cookie( "nonEmpty", "241812619")
                .cookie( "nonEmpty", "167176330")
                .cookie( "nonEmpty", "-899296673")
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
                .cookie( "exploded", "-433134713")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-875471451")
                .cookie( "nonEmpty", "-998027702")
                .cookie( "nonEmpty", "-416884787")
                .cookie( "nonEmpty", "52822052")
                .cookie( "nonEmpty", "-981485662")
                .cookie( "nonEmpty", "-808304676")
                .cookie( "nonEmpty", "-38571485")
                .cookie( "nonEmpty", "1028227969")
                .cookie( "nonEmpty", "-914284260")
                .cookie( "nonEmpty", "966763191")
                .cookie( "nonEmpty", "-502177738")
                .cookie( "nonEmpty", "-971894385")
                .cookie( "nonEmpty", "1028227969")
                .cookie( "nonEmpty", "243362139")
                .cookie( "nonEmpty", "886015674")
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
                .cookie( "exploded", "-80978403")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-355993443")
                .cookie( "nonEmpty", "722905071")
                .cookie( "nonEmpty", "-815462451")
                .cookie( "nonEmpty", "-479573339")
                .cookie( "nonEmpty", "722905071")
                .cookie( "nonEmpty", "-264114826")
                .cookie( "empty", "@`ehD^")
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
                .cookie( "exploded", "-231262562")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-454668425")
                .cookie( "nonEmpty", "1061921353")
                .cookie( "nonEmpty", "-628155895")
                .cookie( "nonEmpty", "-528945091")
                .cookie( "nonEmpty", "1064405701")
                .cookie( "nonEmpty", "815801099")
                .cookie( "nonEmpty", "-324086710")
                .cookie( "nonEmpty", "767721028")
                .cookie( "nonEmpty", "210655818")
                .cookie( "nonEmpty", "-373234722")
                .cookie( "nonEmpty", "144176642")
                .cookie( "nonEmpty", "767721028")
                .cookie( "nonEmpty", "877465447")
                .cookie( "empty", "")
                .cookie( "empty", "-861707288")
                .cookie( "empty", "-401422919")
                .cookie( "empty", "867967939")
                .cookie( "empty", "-4756597")
                .cookie( "empty", "11480252")
                .cookie( "empty", "-656516263")
                .cookie( "empty", "682003222")
                .cookie( "empty", "422868921")
                .cookie( "empty", "-743634271")
                .cookie( "empty", "-460076748")
                .cookie( "empty", "593045149")
                .cookie( "empty", "-327867772")
                .cookie( "empty", "-1048667496")
                .cookie( "empty", "-772474961")
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
                .cookie( "exploded", "-170845080")
                .cookie( "nullable", "")
                .cookie( "empty", "0")
                .cookie( "empty", "595381065")
                .cookie( "empty", "708160850")
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
                .cookie( "exploded", "-326591355")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", null)
                .cookie( "empty", "0")
                .cookie( "empty", "594127837")
                .cookie( "empty", "650149267")
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
                .cookie( "exploded", "-8831889")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "true")
                .cookie( "empty", "0")
                .cookie( "empty", "973247181")
                .cookie( "empty", "-278372590")
                .cookie( "empty", "708694673")
                .cookie( "empty", "-927164328")
                .cookie( "empty", "-126032560")
                .cookie( "empty", "-632433794")
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
                .cookie( "exploded", "-226332117")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-425494140")
                .cookie( "nonEmpty", "-425494140")
                .cookie( "empty", "0")
                .cookie( "empty", "-312808778")
                .cookie( "empty", "1032850358")
                .cookie( "empty", "-257774339")
                .cookie( "empty", "730697012")
                .cookie( "empty", "-596824199")
                .cookie( "empty", "435477289")
                .cookie( "empty", "-534311590")
                .cookie( "empty", "1055473126")
                .cookie( "empty", "-861588701")
                .cookie( "empty", "1023320981")
                .cookie( "empty", "666636319")
                .cookie( "empty", "-844026260")
                .cookie( "empty", "675224604")
                .cookie( "empty", "552768986")
                .cookie( "empty", "566640855")
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
                .cookie( "exploded", "-589768324")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "")
                .cookie( "nonEmpty", "-565342359")
                .cookie( "nonEmpty", "-448786693")
                .cookie( "nonEmpty", "-58817267")
                .cookie( "nonEmpty", "-781624554")
                .cookie( "nonEmpty", "618464323")
                .cookie( "nonEmpty", "-953241066")
                .cookie( "nonEmpty", "718111172")
                .cookie( "nonEmpty", "-189482066")
                .cookie( "nonEmpty", "-577106341")
                .cookie( "nonEmpty", "-405224805")
                .cookie( "nonEmpty", "39267938")
                .cookie( "nonEmpty", "-526959074")
                .cookie( "nonEmpty", "618464323")
                .cookie( "nonEmpty", "38707495")
                .cookie( "nonEmpty", "65911585")
                .cookie( "empty", "0")
                .cookie( "empty", "-289082076")
                .cookie( "empty", "428615712")
                .cookie( "empty", "-457086654")
                .cookie( "empty", "54264425")
                .cookie( "empty", "-569909011")
                .cookie( "empty", "-885726431")
                .cookie( "empty", "1036773847")
                .cookie( "empty", "-42857965")
                .cookie( "empty", "-250682074")
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
                .cookie( "exploded", "-32832183")
                .cookie( "nonEmpty", "-847075096")
                .cookie( "nonEmpty", "-552392873")
                .cookie( "nonEmpty", "10972596")
                .cookie( "nonEmpty", "-521795181")
                .cookie( "nonEmpty", "885610005")
                .cookie( "nonEmpty", "560695496")
                .cookie( "nonEmpty", "24424302")
                .cookie( "nonEmpty", "-826273549")
                .cookie( "nonEmpty", "150436118")
                .cookie( "nonEmpty", "150436118")
                .cookie( "nonEmpty", "-22302229")
                .cookie( "nonEmpty", "-920525169")
                .cookie( "nonEmpty", "431444732")
                .cookie( "nonEmpty", "-990594150")
                .cookie( "empty", "0")
                .cookie( "empty", "336526589")
                .cookie( "empty", "-1061671672")
                .cookie( "empty", "1016279132")
                .cookie( "empty", "531632009")
                .cookie( "empty", "-514380457")
                .cookie( "empty", "700989698")
                .cookie( "empty", "303537315")
                .cookie( "empty", "-812539917")
                .cookie( "empty", "-152658307")
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
                .cookie( "exploded", "-113677740")
                .cookie( "nullable", "-376")
                .cookie( "nonEmpty", "-558179699")
                .cookie( "nonEmpty", "-19995561")
                .cookie( "nonEmpty", "178857939")
                .cookie( "nonEmpty", "-1028087847")
                .cookie( "nonEmpty", "122749270")
                .cookie( "nonEmpty", "220026930")
                .cookie( "nonEmpty", "-52884407")
                .cookie( "nonEmpty", "476609736")
                .cookie( "nonEmpty", "-675683888")
                .cookie( "nonEmpty", "341496686")
                .cookie( "nonEmpty", "-52884407")
                .cookie( "nonEmpty", "-704671765")
                .cookie( "empty", "0")
                .cookie( "empty", "-386655230")
                .cookie( "empty", "-625881874")
                .cookie( "empty", "439773803")
                .cookie( "empty", "-548330101")
                .cookie( "empty", "168939545")
                .cookie( "empty", "414745963")
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
                .cookie( "exploded", "-550623962")
                .cookie( "nullable", "3P8pW")
                .cookie( "nonEmpty", "-831936222")
                .cookie( "nonEmpty", "904738804")
                .cookie( "nonEmpty", "904738804")
                .cookie( "nonEmpty", "-150089467")
                .cookie( "nonEmpty", "-769739125")
                .cookie( "nonEmpty", "750493746")
                .cookie( "nonEmpty", "92431597")
                .cookie( "nonEmpty", "-566009839")
                .cookie( "empty", "0")
                .cookie( "empty", "-944078301")
                .cookie( "empty", "-921254517")
                .cookie( "empty", "525990732")
                .cookie( "empty", "-31450784")
                .cookie( "empty", "694887281")
                .cookie( "empty", "-729465847")
                .cookie( "empty", "-637438237")
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
                .cookie( "nonEmpty", "-822524327")
                .cookie( "nonEmpty", "-103723875")
                .cookie( "nonEmpty", "-614937415")
                .cookie( "nonEmpty", "545765079")
                .cookie( "nonEmpty", "75709701")
                .cookie( "nonEmpty", "90709864")
                .cookie( "nonEmpty", "75709701")
                .cookie( "nonEmpty", "-572330029")
                .cookie( "nonEmpty", "504448472")
                .cookie( "nonEmpty", "937711258")
                .cookie( "nonEmpty", "465531090")
                .cookie( "nonEmpty", "746460045")
                .cookie( "nonEmpty", "-1018827834")
                .cookie( "nonEmpty", "-26387748")
                .cookie( "nonEmpty", "-916832479")
                .cookie( "nonEmpty", "137985347")
                .cookie( "empty", "0")
                .cookie( "empty", "-390843432")
                .cookie( "empty", "1016623117")
                .cookie( "empty", "-996603262")
                .cookie( "empty", "-765133111")
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
                .cookie( "nonEmpty", "-536685720")
                .cookie( "nonEmpty", "847594523")
                .cookie( "nonEmpty", "22216678")
                .cookie( "nonEmpty", "11613030")
                .cookie( "nonEmpty", "-1068138668")
                .cookie( "nonEmpty", "526934627")
                .cookie( "nonEmpty", "-632985551")
                .cookie( "nonEmpty", "-754389747")
                .cookie( "nonEmpty", "608104576")
                .cookie( "nonEmpty", "608104576")
                .cookie( "nonEmpty", "-612049959")
                .cookie( "nonEmpty", "-568846719")
                .cookie( "nonEmpty", "-709522767")
                .cookie( "nonEmpty", "-392101457")
                .cookie( "nonEmpty", "728941678")
                .cookie( "nonEmpty", "-297356726")
                .cookie( "empty", "0")
                .cookie( "empty", "98468685")
                .cookie( "empty", "612485389")
                .cookie( "empty", "-486576637")
                .cookie( "empty", "-364923976")
                .cookie( "empty", "-743256754")
                .cookie( "empty", "-317856439")
                .cookie( "empty", "588622925")
                .cookie( "empty", "778707242")
                .cookie( "empty", "531194630")
                .cookie( "empty", "-144416924")
                .cookie( "empty", "-712919387")
                .cookie( "empty", "-1051906136")
                .cookie( "empty", "-561497123")
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
                .cookie( "exploded", "true")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-785327029")
                .cookie( "nonEmpty", "-14390459")
                .cookie( "nonEmpty", "-785327029")
                .cookie( "nonEmpty", "-1012187196")
                .cookie( "empty", "0")
                .cookie( "empty", "-1010416716")
                .cookie( "empty", "35311021")
                .cookie( "empty", "-859205831")
                .cookie( "empty", "923613126")
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
                .cookie( "exploded", "-480.2")
                .cookie( "nullable", "")
                .cookie( "nonEmpty", "-84839839")
                .cookie( "nonEmpty", "711570101")
                .cookie( "nonEmpty", "-986883856")
                .cookie( "nonEmpty", "-702102430")
                .cookie( "nonEmpty", "-489030819")
                .cookie( "nonEmpty", "-489030819")
                .cookie( "nonEmpty", "-794299005")
                .cookie( "nonEmpty", "476122851")
                .cookie( "nonEmpty", "510712645")
                .cookie( "nonEmpty", "-1015252722")
                .cookie( "empty", "0")
                .cookie( "empty", "-979798962")
                .cookie( "empty", "572291610")
                .cookie( "empty", "-872192705")
                .cookie( "empty", "-653583394")
                .cookie( "empty", "-218610464")
                .cookie( "empty", "-843290735")
                .cookie( "empty", "979139966")
                .cookie( "empty", "-224006432")
                .cookie( "empty", "95229284")
                .cookie( "empty", "576868268")
                .cookie( "empty", "476564567")
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
                .cookie( "deep[jn]", "true")
                .cookie( "deep[nmvdz]", "")
                .cookie( "width", "0")
                .cookie( "height", "0")
                .cookie( "lsvvhdqijdfm", "799")
                .cookie( "nullable", "")
                .cookie( "width", "0")
                .cookie( "height", "0")
                .cookie( "cjxbumikc", "true")
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
                .cookie( "deep[mtublssftsmv]", "")
                .cookie( "deep[awjgycgfpepvpkoy]", "LGc:eMk")
                .cookie( "height", null)
                .cookie( "btmlteoll", "-161.9")
                .cookie( "nullable", "width|0|height|0|arlzcfetyvk|-522.7|istxxdbuqgo|367.3")
                .cookie( "width", null)
                .cookie( "iupeuslpbinxramp", "tJSBGJk,i'#")
                .cookie( "kqlhvzlipfjgqhtq", "-483")
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
                .cookie( "deep[width]", "668024803")
                .cookie( "width", "710842947")
                .cookie( "nullable", "width|")
                .cookie( "width", "307348326")
                .cookie( "height", "596084084")
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
                .cookie( "deep[height]", "766134363")
                .cookie( "deep[rvusgfz]", "273")
                .cookie( "deep[psv]", "308")
                .cookie( "height", "1024381174")
                .cookie( "hbeelbmwsugv", "burzubmxu,true")
                .cookie( "bq", "-973.5")
                .cookie( "nullable", "width|645508785|height||ouclexcaq|-893|vxatqogftmahi|>")
                .cookie( "bdqfakgt", "")
                .cookie( "fyonfchytdemnx", "-849")
                .cookie( "zvrclwjm", "-630.4")
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
                .cookie( "nullable", "height|1073473209")
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
                .cookie( "deep[zknxzlykzu]", "IB*CX8:")
                .cookie( "deep[sbu]", "889")
                .cookie( "deep[cygxg]", "537.4")
                .cookie( "height", "0")
                .cookie( "dwvesohqyqdpss", "657")
                .cookie( "nullable", "width|0|ubirwu|true|pumsjwovayk|KEij|gyvgwfbwruoxrms|true")
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
                .cookie( "deep[rxtsnmciauydpvv]", "765.7")
                .cookie( "deep[i]", "-61")
                .cookie( "deep[hciepiwkjadux]", "true")
                .cookie( "height", "0")
                .cookie( "kbcbvmn", "839")
                .cookie( "o", "`*ZjLziI")
                .cookie( "nullable", "width|0|chvjffybrobueap|true")
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
                .cookie( "deep[hwzqysvmcbh]", "-680.1")
                .cookie( "deep[b]", "")
                .cookie( "height", "0")
                .cookie( "rbbbs", "8f,Aew,HD`")
                .cookie( "ve", "ftdsl,156.3,illrbyqf,829,anffxfxv,true")
                .cookie( "wvozdcudalh", "388.4")
                .cookie( "nullable", "width|0|zuliplqfhtxrxwnq|HmzNdH-,A/$La,9{QX}H|rmqmgx|254|lyid|#>yQj~f,Wi>T,b]")
                .cookie( "nonEmpty", "439.8")
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
                .cookie( "deep[zxkylqa]", "")
                .cookie( "deep[qzzqmnebhsy]", "gawycgwlzef,6,xtbniv,+',bvho,(krI&$")
                .cookie( "height", "0")
                .cookie( "mmmbhkq", "G_,%d")
                .cookie( "nullable", "width|0|uljthfeimgqjv|-389|hc|-394.1|wqcxnhoqdm|")
                .cookie( "width", "")
                .cookie( "aein", "L-U^")
                .cookie( "hkuwspwwflredjl", "true")
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
                .cookie( "deep[vxvanxt]", "true")
                .cookie( "deep[muq]", "-1013.0")
                .cookie( "deep[dnnmzsfsnslks]", "hjn,)v,ydkiufbaookrgk,658,kpedmmpydaapwmj,SQ")
                .cookie( "height", "0")
                .cookie( "jpxlbhl", "t1z!o")
                .cookie( "nullable", "width|0|rbrzdiqe|")
                .cookie( "width", "-1")
                .cookie( "dguenteol", "true")
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
                .cookie( "deep[opjkedbtad]", "cszzagukrwbkazfh,true,vfwqlwqjxezbznp,-467,tnj,qQ@6r,{HDH")
                .cookie( "deep[uzpytbtvywdwwv]", "Z>G{?8#")
                .cookie( "deep[ukd]", "Yc*B8j")
                .cookie( "height", "0")
                .cookie( "fbzfzovbuwwcmzm", "true")
                .cookie( "ntxmvjpyenchhbc", "lshktdh,Tr,$m,x%N7G,vyponittzsmj,gP,=WVC4',Gu.]ohC%")
                .cookie( "j", "]?lfa.|}")
                .cookie( "nullable", "width|0|ezy|k-|nauccuk|-329")
                .cookie( "height", "")
                .cookie( "pbyfcbvtpmxrgib", "e,-789.0,iiknyex,-891.5")
                .cookie( "chklbagb", ",smPety~")
                .cookie( "ihrkynjjqpod", "-666")
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
                .cookie( "deep[fc]", "r}@,]QO")
                .cookie( "height", "0")
                .cookie( "tcon", "odxuplhukss,268")
                .cookie( "nac", "")
                .cookie( "nullable", "width|0|fpfqjsovrys|340")
                .cookie( "height", "-1")
                .cookie( "kkyjsdysbnulxvp", "bpykwnexzvi,b")
                .cookie( "rffbvhb", "Ho")
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
                .cookie( "deep[mqvq]", "-985.1")
                .cookie( "deep[uccepkssu]", "KbdkR")
                .cookie( "deep[axmnfxh]", "753.0")
                .cookie( "height", "0")
                .cookie( "bythamamnhulptfu", "")
                .cookie( "gcmypcsctzqssmy", "-616")
                .cookie( "gjqkercxbeafsqgs", "316.7")
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
                .cookie( "deep[jyvqmtocpufzmla]", "486.9")
                .cookie( "height", "0")
                .cookie( "rnigjjpmeqca", "D-,jGLUs0,4_GPK5")
                .cookie( "xdwaei", "=X][3V")
                .cookie( "nullable", "709")
                .cookie( "fywzrgbfgtlhbcdy", "")
                .cookie( "ylyiiyrmsaiys", "433.3")
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
                .cookie( "deep[hyv]", "true")
                .cookie( "height", "0")
                .cookie( "liqd", "190")
                .cookie( "nullable", "width||shmafji|true|ghoznzk|Vc2+")
                .cookie( "ztulkvgosvrvh", "-331.5")
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
                .cookie( "deep[nz]", "true")
                .cookie( "deep[ziwmceakjasmbut]", "")
                .cookie( "height", "0")
                .cookie( "jplrzxwau", "N")
                .cookie( "efsy", "")
                .cookie( "nullable", "width|-1|lhmwppmdhcdzw|knahxuuawfksi,il~el,je^/|fqb|{,L9Jt^{,re|zsqvhggslhql|true")
                .cookie( "bgohofefqgepwa", "$IDQL")
                .cookie( "cpxlpuhytigshg", "409")
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
                .cookie( "deep[yov]", "-883.8")
                .cookie( "deep[mqlzzcufvgbw]", "")
                .cookie( "deep[vln]", "917")
                .cookie( "height", "0")
                .cookie( "omul", "-810")
                .cookie( "bx", "")
                .cookie( "nullable", "width|0|height|677.0|dpictbqe|:v:,jYr&%n,:}`]N9|vejzubejuohaak|true|spcerbzupbrnol|ecjgyhzprmale,-10,u,x,ewxyfitslnpvap,-!,{~")
                .cookie( "fcivxmd", "true")
                .cookie( "iknsqzybchynsuyk", ".&L8!C,'--Zlt_,]")
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
                .cookie( "deep[uzzyeiqbihrwxj]", "-423.3")
                .cookie( "height", "0")
                .cookie( "dyojowoxkwgjkw", "855.4")
                .cookie( "ijdvagzc", "true")
                .cookie( "nullable", "width|0|height|-1|jwy|732.1")
                .cookie( "jgsfthbiisenb", "h8")
                .cookie( "ghxeuaqevjmepqj", "")
                .cookie( "avvcdzimxeyeenmt", "294")
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
                .cookie( "deep[gozfwkpyrknpo]", "J~:,iKJRI{0")
                .cookie( "deep[osqwnzcvwklx]", "172.5")
                .cookie( "nullable", "width|0|yelsn|sbqildbygog,Z+cM8Z1,_s9C8,B7-:,cwmuuiuyqif,true,smfcxmazya,-143|whszvkked|807")
                .cookie( "igqv", "841")
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
                .cookie( "deep[y]", "dkbpo,true,voodzhdxgihaftw,434")
                .cookie( "exploded", null)
                .cookie( "nullable", "width|0|pwofyomatsvos|412.3")
                .cookie( "lb", "T")
                .cookie( "dvrkvldkqpunez", "-411")
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
                .cookie( "deep[qqlrmxnfyyh]", "GAA")
                .cookie( "deep[ncehma]", "-479")
                .cookie( "deep[hjcsamzouzzazblf]", "43.2")
                .cookie( "exploded", "791")
                .cookie( "nullable", "width|0|oijprlsgharbc|z!JRP4jU|j|oG~&XVO|ieyeelstdh|-131")
                .cookie( "dfnlner", "NmQ{<,?^?t%W-,o|:|e=")
                .cookie( "iplvnpcpanynhupp", "")
                .cookie( "in", "true")
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
                .cookie( "deep[bawsjbvtnfu]", "lasyotifnyflohoq,-641,dj,vN~'F'")
                .cookie( "deep[wffhxopaggucptky]", "")
                .cookie( "width", "true")
                .cookie( "height", "0")
                .cookie( "nmotnu", "nfatnpfkzescku,-805,ynpftdhav,-18.9")
                .cookie( "audfplnefflr", "true")
                .cookie( "hqxuox", "507.5")
                .cookie( "nullable", "width|0|tfuikqxp|*okoL.@,l^#2QP7c,=h|d|-864")
                .cookie( "gbpzhriyhk", "F4T+z0")
                .cookie( "sheilu", "dlrrhul,190,yac,~(Kf")
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
                .cookie( "deep[ddcxaio]", "198")
                .cookie( "deep[axtvelcvzmbeuuyr]", "aFi,(P4")
                .cookie( "width", "-1")
                .cookie( "height", "0")
                .cookie( "hwjjhefgr", "L)BB")
                .cookie( "tqsww", "180")
                .cookie( "aqutra", "799.1")
                .cookie( "nullable", "width|0|cuxtnsdenodk|true|jsxakfe|lQ-j,[|pyab|true")
                .cookie( "qjsxwbre", "_Z*7<sU6")
                .cookie( "lpyzbhxskn", "true")
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
                .cookie( "deep[uylfhbnchzw]", "-295.4")
                .cookie( "deep[yclpvt]", "djgjahq,06`?k(K&,ykpzcuolbxup,true")
                .cookie( "deep[znyvjspsvrbtyixk]", "58!G-E,/&")
                .cookie( "height", "-204.0")
                .cookie( "mklxacwjydjr", "xisuppwzhriihjij,5|zgEh,enkyjavvvwoqnqu,801,nwifvs,-862.2")
                .cookie( "nullable", "width|0|zug|true|syjgczpyctcpsg|true|chdu|true")
                .cookie( "whkp", "true")
                .cookie( "smixkvtog", "989")
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
                .cookie( "deep[bursbrbe]", "ouv?Oz!,&?2)")
                .cookie( "deep[d]", "-332.8")
                .cookie( "deep[iwkjpyn]", "true")
                .cookie( "height", "-1")
                .cookie( "wmevaujj", "jyC(|nYf")
                .cookie( "rpewulct", "nSD,g|:',b1B")
                .cookie( "nullable", "width|0|zql|j_/u4cA7|bowwpvdge|-583")
                .cookie( "ezpjsxkdsdrrd", "I|2")
                .cookie( "kncikulyono", "q,3IZi,kwmemwvgvx,true")
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
                .cookie( "ukmfyjxe", "blkgbjdsgawpfcr,<,dnilsks,true,h,876")
                .cookie( "nullable", "width|0|wso|-174|zd|true|lyrdopr|7Sl?,,q~MFG3A")
                .cookie( "dfp", "-263")
                .cookie( "fvkmlbdaa", "true")
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
                .cookie( "vmirderznamaoth", "-228.7")
                .cookie( "nullable", "width|0|focjjnwnmcu|1}`9(->r|wzyjruvco|-773")
                .cookie( "upwgeyoogftky", "-370.9")
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
                .cookie( "deep", "true")
                .cookie( "height", "0")
                .cookie( "l", "ioulexo,457.9")
                .cookie( "nullable", "width|0|trtdvfughutwr|%InM,&icP,t|eekmmemqlf|")
                .cookie( "gnp", "803.2")
                .cookie( "iemgnfln", "njqzjf,217.7,zooprtaznxsljzxn,mI9CO|,vgvuhaodbrm,566")
                .cookie( "tz", "nfnlqmb,911.0,pacqbaldlcxuedzb,330")
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
                .cookie( "deep[width]", "'u9i+")
                .cookie( "deep[height]", "0")
                .cookie( "deep[egklajzrluqxvvgf]", "yqo,-986,cqjnqdxphhuxzon,468.2")
                .cookie( "height", "0")
                .cookie( "amvpnpcgxd", "83")
                .cookie( "tigenogp", "true")
                .cookie( "kn", "true")
                .cookie( "nullable", "width|0|yexpaodg|true|vbthboqucccr|-421.0")
                .cookie( "gaeuctuppysolda", "true")
                .cookie( "fjbofvxwgzykrfx", "BYB")
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
                .cookie( "deep[ifszplmoegzxn]", "sh,~X0HTx,)V,hzgpkmkdf,true,yjvjamlhyizyfd,333")
                .cookie( "deep[gfuvjzfyermzq]", "#|}no")
                .cookie( "height", "0")
                .cookie( "v", "787")
                .cookie( "rtwpwqfsqsaovj", "")
                .cookie( "zzjakk", "-707.4")
                .cookie( "nullable", "width|0|gy|-477|x|310.3")
                .cookie( "bweirse", "R<P@")
                .cookie( "trxfxvhozgiqrlm", "-694")
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
                .cookie( "deep[height]", "`~]f")
                .cookie( "deep[mkesywzpjdebf]", "sfndwkhfkehtqurs,9Up^4~1c")
                .cookie( "deep[sjbhfbat]", "%+%TC+c")
                .cookie( "height", "0")
                .cookie( "cuaaq", "-829")
                .cookie( "frdn", "980")
                .cookie( "nullable", "width|0|ay|")
                .cookie( "zf", "X5)xr")
                .cookie( "ztlnoslgqpcrnkyf", "")
                .cookie( "oihsmixndpmzbwrp", "")
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
                .cookie( "deep[igow]", "661.8")
                .cookie( "height", "0")
                .cookie( "klyliqydbh", "-654")
                .cookie( "nullable", "width|0|zfxnvdzlpkj|true|klngqtoypmbku|872.4")
                .cookie( "un", "-793")
                .cookie( "sxxcm", "mUU`S0^],{H,L1{|6ujf")
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
                .cookie( "nullable", "&<j}52#>-O6^Y26%A_EAMFLXAid<r?8xXBUS'1hY=i3OByn=HStv=y&_m&Zx5QhVx$8u2+R)mo}Z`F*@v!U'n||$fcBQi{bV0{^6^Z9cHy+DyOY+9gFyw#/5%~<g.kyzz*$:Q~q9(rO^{SJN}V$cCu*ZqWPsO:Af[:GF)_1ctcut_iJoH_Z)j$5(AK(acAMQKLBf3[]'kLss++uy/%175`6tNy")
                .cookie( "nonEmpty", "[2T")
                .cookie( "empty", "L@|b+cw]h=)F1reH270/3<HA^I(:GIHQ)IPo*zK#_acf/&(7c9tsY4I_nrL*Q*s*l!2<pfbP+M}Gd.F+=jtY0~1Tm7^>cIEw=tCYbK:&bn<=w><RxZ70Vk=1oV(")
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
                .cookie( "nonEmpty", "V(~P(Uy)ypnr0]f4XHTthGYC/UF8P2%!8f5P:4h%VWYTs:!m3F9=9=Kl'+/UZy6V")
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
                .cookie( "nonEmpty", "5[%")
                .cookie( "empty", "&zxC")
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
                .cookie( "nullable", "]XhOM'T*qgb:h9E6<trfZQZQrLiLI>$>F>g[@tNKGfC)p_`5DOv8pSU&S[h}V%R0R@}@lHszlD*{)kf'*>&VoO?=:/'t[v{WUk+xmv~(/e6:s8$F8^!|Do%}A2h(R4-lu>$QSd%5|[Z)I&&:'I3ZMsCvVEO6T+%(#5Pnyef6bKd=y@Q'7Ve3z'R>[M~ML61W/mbcGTyR9FD{*#N2goef6zEsXK}|<%kVU2='<0dSmIh")
                .cookie( "nonEmpty", "?(0p@'9P7=`z|^0@1-<i_$7F{0OJZMMm67%&|z7}RV]2+EL=`X`d/23{Qqt4sKK~c~RH837Iw.rA8Q{_m^J")
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
                .cookie( "nullable", "UB[7c>3|!.UBXs?H{KTVIZGp{XP7^R1:uWWL-G^XnhmJ'k>V:x/_x}bK4~{-rwwE.mt9p[4y!.0y1r5Ul>gi&q`~'%x8ICuZwmPto1qGbgJ3(Uw&a9mx2LA+VD8VS1nS&b7QN1EfZ&>pw+^?Hrt=vQw2ba=6rw8i|N/")
                .cookie( "nonEmpty", "2O_>Ur~<V%35~Q~t*iQKZnn'&5%rJStm6#o7+kKM<AXNV.zL6(?~z_q|Q`Er354eEHd@vf<`0e3H3q=U@rM&*T/**#X]O1.]wwBkYH3PebC!v*R:6Opz@aA^3^qRv<f~!$zgQ6`GA(!dfz/2@{IIR9Of6rIbTC[}#P1+N06!]Hglg^Q}j4@Xy<w&`yaA8g&F_aqbp`ytX=JKbgj{NZt?numfm#.B~7q5<x:|i7a)L*_Cb`EypbSXQ1")
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
                .cookie( "nullable", "FSTggl`DoD2wn~FC*-.>w9hEKZ8{ONl~:_)Sen'&nR+?o|k?k$%F^*Rr.#`Iveom!?#yc]IAe]yBk|]0XthyTk4O}$nUAt9Mby=gQbc'WOd09c~_Qv")
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
                .cookie( "nullable", "7.+B+_/lP:y)_c~[*np'@4)5e|)m")
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
                .cookie( "nullable", "z#?`H7l*U8ty2vaN!FBysN!w|lDyk{*Ub@@")
                .cookie( "nonEmpty", "7e")
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
                .cookie( "nonEmpty", "L(SPN_-OIyte#_)1KPBjvQ3}cM)J^NP=rDoG%$!Bp6MtNL^iv//66Or}lk?OR!T&@&T?R-v1l`j+=K/[zfl1dvTw~/'Yp._ET_Jux?Ll")
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
                .queryParam( "nonEmpty", "-997025196")
                .queryParam( "nonEmpty", "682566738")
                .queryParam( "nonEmpty", "-672135748")
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
                .queryParam( "exploded", "-353282278")
                .queryParam( "nullable", (String) null)
                .queryParam( "nonEmpty", (String) null)
                .queryParam( "nonEmpty", "-56674311")
                .queryParam( "nonEmpty", "570875324")
                .queryParam( "nonEmpty", "862239138")
                .queryParam( "nonEmpty", "-762830570")
                .queryParam( "nonEmpty", "578387582")
                .queryParam( "nonEmpty", "-667734390")
                .queryParam( "nonEmpty", "52888943")
                .queryParam( "nonEmpty", "-613703947")
                .queryParam( "nonEmpty", "570875324")
                .queryParam( "nonEmpty", "-178922770")
                .queryParam( "nonEmpty", "938659273")
                .queryParam( "empty", "-310726322")
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
                .queryParam( "exploded", "-123379548")
                .queryParam( "exploded", "833605195")
                .queryParam( "exploded", "-470221954")
                .queryParam( "exploded", "253492583")
                .queryParam( "exploded", "489860160")
                .queryParam( "exploded", "927002685")
                .queryParam( "exploded", "-374928336")
                .queryParam( "exploded", "324498561")
                .queryParam( "exploded", "-447569366")
                .queryParam( "exploded", "-246773701")
                .queryParam( "exploded", "391059911")
                .queryParam( "exploded", "1051980808")
                .queryParam( "exploded", "-587451820")
                .queryParam( "nullable", "-606844601")
                .queryParam( "nonEmpty", "0")
                .queryParam( "nonEmpty", "-423775159")
                .queryParam( "nonEmpty", "-847400667")
                .queryParam( "empty", (String) null)
                .queryParam( "empty", "-986748104")
                .queryParam( "empty", "1041524851")
                .queryParam( "empty", "536130786")
                .queryParam( "empty", "779822933")
                .queryParam( "empty", "1022475891")
                .queryParam( "empty", "1071792615")
                .queryParam( "empty", "1003304817")
                .queryParam( "empty", "-371443331")
                .queryParam( "empty", "-118750903")
                .queryParam( "empty", "-743637375")
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
                .queryParam( "nullable", "617143212")
                .queryParam( "nullable", "-190189331")
                .queryParam( "nullable", "288416967")
                .queryParam( "nullable", "-834544207")
                .queryParam( "nullable", "184418481")
                .queryParam( "nullable", "808988688")
                .queryParam( "nullable", "-372617346")
                .queryParam( "nonEmpty", "488065961")
                .queryParam( "nonEmpty", "72596763")
                .queryParam( "nonEmpty", "-296017831")
                .queryParam( "nonEmpty", "213256445")
                .queryParam( "nonEmpty", "72596763")
                .queryParam( "nonEmpty", "510153224")
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
                .queryParam( "nonEmpty", "-902694470")
                .queryParam( "nonEmpty", "-243357730")
                .queryParam( "nonEmpty", "990367744")
                .queryParam( "empty", "463221856")
                .queryParam( "empty", "463221856")
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
                .queryParam( "exploded", "253195088")
                .queryParam( "exploded", "253195088")
                .queryParam( "nullable", "0")
                .queryParam( "nonEmpty", "0")
                .queryParam( "nonEmpty", "0")
                .queryParam( "nonEmpty", "64986756")
                .queryParam( "nonEmpty", "-737295796")
                .queryParam( "nonEmpty", "-147385826")
                .queryParam( "nonEmpty", "431083565")
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
                .queryParam( "nullable", "70530052")
                .queryParam( "nullable", "-1056340534")
                .queryParam( "nullable", "-1056340534")
                .queryParam( "nonEmpty", "284494660")
                .queryParam( "nonEmpty", "-287886212")
                .queryParam( "nonEmpty", "646483382")
                .queryParam( "empty", "-104528854")
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
                .queryParam( "exploded", "-79104789")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-650204163")
                .queryParam( "nonEmpty", "24282505")
                .queryParam( "nonEmpty", "24282505")
                .queryParam( "nonEmpty", "691777861")
                .queryParam( "nonEmpty", "-224601062")
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
                .queryParam( "exploded", "-544260731")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-531325945")
                .queryParam( "nonEmpty", "331672102")
                .queryParam( "nonEmpty", "-893662531")
                .queryParam( "nonEmpty", "-893662531")
                .queryParam( "nonEmpty", "-874186250")
                .queryParam( "nonEmpty", "-213851462")
                .queryParam( "nonEmpty", "-772744928")
                .queryParam( "nonEmpty", "-690599939")
                .queryParam( "nonEmpty", "-661964290")
                .queryParam( "nonEmpty", "-468691461")
                .queryParam( "nonEmpty", "-228899221")
                .queryParam( "nonEmpty", "85425210")
                .queryParam( "nonEmpty", "-69920141")
                .queryParam( "nonEmpty", "-57884003")
                .queryParam( "nonEmpty", "-247792650")
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
                .queryParam( "exploded", "-942023291")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-368116076")
                .queryParam( "nonEmpty", "237832009")
                .queryParam( "nonEmpty", "512548671")
                .queryParam( "nonEmpty", "237832009")
                .queryParam( "nonEmpty", "395623599")
                .queryParam( "empty", "true")
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
                .queryParam( "exploded", "-61567366")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-500636149")
                .queryParam( "nonEmpty", "844025164")
                .queryParam( "nonEmpty", "312440747")
                .queryParam( "nonEmpty", "-984257839")
                .queryParam( "nonEmpty", "-96976226")
                .queryParam( "nonEmpty", "-446897229")
                .queryParam( "nonEmpty", "844025164")
                .queryParam( "nonEmpty", "-919402968")
                .queryParam( "nonEmpty", "-369047197")
                .queryParam( "nonEmpty", "-676943225")
                .queryParam( "empty", "!}Zg4Db")
                .queryParam( "empty", "149655144")
                .queryParam( "empty", "-792634695")
                .queryParam( "empty", "-781709390")
                .queryParam( "empty", "-230257981")
                .queryParam( "empty", "467318145")
                .queryParam( "empty", "63782741")
                .queryParam( "empty", "-144505828")
                .queryParam( "empty", "326389262")
                .queryParam( "empty", "-962114998")
                .queryParam( "empty", "-689638676")
                .queryParam( "empty", "-205273198")
                .queryParam( "empty", "199557483")
                .queryParam( "empty", "1038458535")
                .queryParam( "empty", "633733070")
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
                .queryParam( "exploded", "-356078792")
                .queryParam( "nullable", "")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-600301358")
                .queryParam( "empty", "277723967")
                .queryParam( "empty", "-519514028")
                .queryParam( "empty", "-431380027")
                .queryParam( "empty", "-431535649")
                .queryParam( "empty", "1070715438")
                .queryParam( "empty", "-951416082")
                .queryParam( "empty", "-655033002")
                .queryParam( "empty", "896903913")
                .queryParam( "empty", "-438984672")
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
                .queryParam( "exploded", "-24372224")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", (String) null)
                .queryParam( "empty", "0")
                .queryParam( "empty", "669999838")
                .queryParam( "empty", "455791148")
                .queryParam( "empty", "-591448741")
                .queryParam( "empty", "220244133")
                .queryParam( "empty", "-216078619")
                .queryParam( "empty", "338819103")
                .queryParam( "empty", "-29284074")
                .queryParam( "empty", "-946180665")
                .queryParam( "empty", "738568039")
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
                .queryParam( "exploded", "-464009792")
                .queryParam( "nullable", "")
                .queryParam( "empty", "0")
                .queryParam( "empty", "364242477")
                .queryParam( "empty", "222537679")
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
                .queryParam( "exploded", "-987010397")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-577610107")
                .queryParam( "nonEmpty", "-577610107")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-123199403")
                .queryParam( "empty", "-100343556")
                .queryParam( "empty", "423544022")
                .queryParam( "empty", "-377978137")
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
                .queryParam( "exploded", "-530869338")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "fqqu,V5")
                .queryParam( "nonEmpty", "942935293")
                .queryParam( "nonEmpty", "942935293")
                .queryParam( "nonEmpty", "286177554")
                .queryParam( "nonEmpty", "737625409")
                .queryParam( "nonEmpty", "-235116009")
                .queryParam( "nonEmpty", "277302202")
                .queryParam( "nonEmpty", "628047030")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-347740067")
                .queryParam( "empty", "-603146024")
                .queryParam( "empty", "845441783")
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
                .queryParam( "exploded", "-444053408")
                .queryParam( "nonEmpty", "-110569375")
                .queryParam( "nonEmpty", "-291851879")
                .queryParam( "nonEmpty", "-35377710")
                .queryParam( "nonEmpty", "-817490970")
                .queryParam( "nonEmpty", "976486338")
                .queryParam( "nonEmpty", "214023967")
                .queryParam( "nonEmpty", "-948180378")
                .queryParam( "nonEmpty", "209890861")
                .queryParam( "nonEmpty", "760663638")
                .queryParam( "nonEmpty", "976486338")
                .queryParam( "nonEmpty", "-109502115")
                .queryParam( "nonEmpty", "-678263805")
                .queryParam( "nonEmpty", "-922061430")
                .queryParam( "nonEmpty", "-517379009")
                .queryParam( "nonEmpty", "175251215")
                .queryParam( "nonEmpty", "-64108784")
                .queryParam( "empty", "0")
                .queryParam( "empty", "431631294")
                .queryParam( "empty", "866818419")
                .queryParam( "empty", "14256305")
                .queryParam( "empty", "-100154907")
                .queryParam( "empty", "771207568")
                .queryParam( "empty", "-968541275")
                .queryParam( "empty", "-588912598")
                .queryParam( "empty", "67773507")
                .queryParam( "empty", "776689436")
                .queryParam( "empty", "676358452")
                .queryParam( "empty", "386231642")
                .queryParam( "empty", "205979444")
                .queryParam( "empty", "326554534")
                .queryParam( "empty", "-767405700")
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
                .queryParam( "exploded", "-475616204")
                .queryParam( "nonEmpty", "-809255702")
                .queryParam( "nonEmpty", "1048832588")
                .queryParam( "nonEmpty", "-803423449")
                .queryParam( "nonEmpty", "-198528691")
                .queryParam( "nonEmpty", "-181798054")
                .queryParam( "nonEmpty", "212585086")
                .queryParam( "nonEmpty", "-118362829")
                .queryParam( "nonEmpty", "672361681")
                .queryParam( "nonEmpty", "-8514792")
                .queryParam( "nonEmpty", "106798013")
                .queryParam( "nonEmpty", "1048832588")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-1016708073")
                .queryParam( "empty", "-170591598")
                .queryParam( "empty", "-361800728")
                .queryParam( "empty", "871159705")
                .queryParam( "empty", "-664139660")
                .queryParam( "empty", "-775250209")
                .queryParam( "empty", "635746240")
                .queryParam( "empty", "538082130")
                .queryParam( "empty", "-817641102")
                .queryParam( "empty", "-301299592")
                .queryParam( "empty", "-799157530")
                .queryParam( "empty", "-632734795")
                .queryParam( "empty", "645022826")
                .queryParam( "empty", "-648455253")
                .queryParam( "empty", "653103594")
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
                .queryParam( "exploded", "-120441688")
                .queryParam( "nullable", " Kg,K4")
                .queryParam( "nonEmpty", "-318154028")
                .queryParam( "nonEmpty", "-693697975")
                .queryParam( "nonEmpty", "12392669")
                .queryParam( "nonEmpty", "-992980305")
                .queryParam( "nonEmpty", "-912077780")
                .queryParam( "nonEmpty", "910975193")
                .queryParam( "nonEmpty", "-30653322")
                .queryParam( "nonEmpty", "-1030436900")
                .queryParam( "nonEmpty", "-566453808")
                .queryParam( "nonEmpty", "-439357068")
                .queryParam( "nonEmpty", "371263582")
                .queryParam( "nonEmpty", "-643953814")
                .queryParam( "nonEmpty", "-643953814")
                .queryParam( "nonEmpty", "-427350919")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-875743547")
                .queryParam( "empty", "336532200")
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
                .queryParam( "nonEmpty", "-1058788367")
                .queryParam( "nonEmpty", "388564378")
                .queryParam( "nonEmpty", "-600035346")
                .queryParam( "nonEmpty", "-121366918")
                .queryParam( "nonEmpty", "283848726")
                .queryParam( "nonEmpty", "-649527652")
                .queryParam( "nonEmpty", "-286928353")
                .queryParam( "nonEmpty", "-679604337")
                .queryParam( "nonEmpty", "499159770")
                .queryParam( "nonEmpty", "751955244")
                .queryParam( "nonEmpty", "751955244")
                .queryParam( "nonEmpty", "18884480")
                .queryParam( "nonEmpty", "251659881")
                .queryParam( "nonEmpty", "-891515753")
                .queryParam( "empty", "0")
                .queryParam( "empty", "659509409")
                .queryParam( "empty", "689895472")
                .queryParam( "empty", "-1056437228")
                .queryParam( "empty", "-429396004")
                .queryParam( "empty", "552542683")
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
                .queryParam( "nonEmpty", "-689465621")
                .queryParam( "nonEmpty", "988902363")
                .queryParam( "nonEmpty", "713716828")
                .queryParam( "nonEmpty", "989188662")
                .queryParam( "nonEmpty", "-399328571")
                .queryParam( "nonEmpty", "375671116")
                .queryParam( "nonEmpty", "-340563125")
                .queryParam( "nonEmpty", "779774516")
                .queryParam( "nonEmpty", "777661267")
                .queryParam( "nonEmpty", "-515904133")
                .queryParam( "nonEmpty", "546784706")
                .queryParam( "nonEmpty", "713716828")
                .queryParam( "nonEmpty", "516982739")
                .queryParam( "nonEmpty", "808009824")
                .queryParam( "nonEmpty", "764180430")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-517860973")
                .queryParam( "empty", "24545181")
                .queryParam( "empty", "245524847")
                .queryParam( "empty", "135750345")
                .queryParam( "empty", "373503629")
                .queryParam( "empty", "405701817")
                .queryParam( "empty", "-986262316")
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
                .queryParam( "exploded", ")Ss")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-733900912")
                .queryParam( "nonEmpty", "-1069292604")
                .queryParam( "nonEmpty", "-910418433")
                .queryParam( "nonEmpty", "-1025908321")
                .queryParam( "nonEmpty", "-1057249717")
                .queryParam( "nonEmpty", "-497287594")
                .queryParam( "nonEmpty", "784348174")
                .queryParam( "nonEmpty", "-339241431")
                .queryParam( "nonEmpty", "257462113")
                .queryParam( "nonEmpty", "605331926")
                .queryParam( "nonEmpty", "781448125")
                .queryParam( "nonEmpty", "-236118974")
                .queryParam( "nonEmpty", "432047252")
                .queryParam( "nonEmpty", "-910418433")
                .queryParam( "nonEmpty", "-811395139")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-576985121")
                .queryParam( "empty", "355412643")
                .queryParam( "empty", "-309930293")
                .queryParam( "empty", "-1070683276")
                .queryParam( "empty", "-980269335")
                .queryParam( "empty", "207601164")
                .queryParam( "empty", "1055793375")
                .queryParam( "empty", "-227075240")
                .queryParam( "empty", "423995712")
                .queryParam( "empty", "458832875")
                .queryParam( "empty", "381143298")
                .queryParam( "empty", "-732586401")
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
                .queryParam( "exploded", "C7")
                .queryParam( "nullable", "")
                .queryParam( "nonEmpty", "-267975450")
                .queryParam( "nonEmpty", "-845743801")
                .queryParam( "nonEmpty", "733221343")
                .queryParam( "nonEmpty", "491337161")
                .queryParam( "nonEmpty", "361346627")
                .queryParam( "nonEmpty", "861528453")
                .queryParam( "nonEmpty", "68160379")
                .queryParam( "nonEmpty", "930157120")
                .queryParam( "nonEmpty", "930157120")
                .queryParam( "nonEmpty", "-813892385")
                .queryParam( "empty", "0")
                .queryParam( "empty", "583896266")
                .queryParam( "empty", "13737467")
                .queryParam( "empty", "155521781")
                .queryParam( "empty", "-574151990")
                .queryParam( "empty", "-932885110")
                .queryParam( "empty", "-246789679")
                .queryParam( "empty", "382604220")
                .queryParam( "empty", "-704666886")
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
                .queryParam( "deep[ee]", "smtfokculr,true,vljgvo,-744")
                .queryParam( "width", "0")
                .queryParam( "height", "0")
                .queryParam( "kouuvoumqzbsp", "fzdfwarznxn,3,s,XN$wvKQ,G\"UBx")
                .queryParam( "hzynrgada", "true")
                .queryParam( "lzcginjm", "Re]QgS")
                .queryParam( "nullable", "")
                .queryParam( "width", "0")
                .queryParam( "height", "0")
                .queryParam( "rrqkqpotn", ":EYv3qS")
                .queryParam( "vidzfcqisrp", "-948")
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
                .queryParam( "deep[nynqnaseiifqpt]", "xlbpgpjzgip,,hrpyxrs,eOMT,nmlgkcg,946")
                .queryParam( "deep[wrbd]", "|x)7 i,Fs/fI,)U^d)o")
                .queryParam( "deep[skh]", "true")
                .queryParam( "height", (String) null)
                .queryParam( "smbpawlmo", "V]j,U~,g=dDBY2")
                .queryParam( "pctiixrlxpnebum", "-100")
                .queryParam( "nullable", "width 0 height 0 xpx 350")
                .queryParam( "width", (String) null)
                .queryParam( "kg", "}OtD")
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
                .queryParam( "deep[width]", "566724558")
                .queryParam( "width", "815855973")
                .queryParam( "nullable", "width ")
                .queryParam( "width", "1009632")
                .queryParam( "height", "404022269")
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
                .queryParam( "deep[height]", "79758990")
                .queryParam( "deep[muiwmpc]", "|Rx^X<fl,>UO, -:}'OT7")
                .queryParam( "deep[dqbjegpbzik]", "-640")
                .queryParam( "deep[wjfngaflvimvtz]", "r{l~f-")
                .queryParam( "height", "725867407")
                .queryParam( "ooiiriic", "R,")
                .queryParam( "efs", ",Fn2!,V^~0]\\K#")
                .queryParam( "oakxaueccziz", "a ]DkT,")
                .queryParam( "nullable", "width 617163379 height  yylsoot true fytc hqtxtzckxra,")
                .queryParam( "wo", ",MI!>2,oP<x&V't")
                .queryParam( "lsjvuncp", "507.4")
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
                .queryParam( "nullable", "height 892882874")
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
                .queryParam( "deep[cgxzg]", "hyl")
                .queryParam( "height", "0")
                .queryParam( "zqcyuzpqgd", "396")
                .queryParam( "eyv", "")
                .queryParam( "ywhjfqmrfxo", "+qt:p_,4B,rt)(")
                .queryParam( "nullable", "width 0 cmiwtimtss 625")
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
                .queryParam( "deep[infbrccbhsjazlh]", "true")
                .queryParam( "height", "0")
                .queryParam( "nskmtqnuzuev", "v]B,VV /n\"")
                .queryParam( "nullable", "width 0 dzbskvujyscfvgrm g|b wyjecq 495")
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
                .queryParam( "deep[gqiyaxcxnywozlfq]", "eurqbz,K93L=,jfj,true")
                .queryParam( "height", "0")
                .queryParam( "tbvtinkhicsqv", "hfzuqj,,okqfdgdgcqziyo,?,:pi,itvub,106.7")
                .queryParam( "envhzdcpxmc", "lhxfitjpx,1017.6,neqhti,<Hi|o^EI")
                .queryParam( "egze", "iahogihezbuaxgey,-170.8")
                .queryParam( "nullable", "width 0 ufui -630 vsgzwwwycydl true grelys ")
                .queryParam( "nonEmpty", "-104")
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
                .queryParam( "deep[qrkn]", "WIyoq")
                .queryParam( "height", "0")
                .queryParam( "asxdztml", "940")
                .queryParam( "itfmpnvotfjzzss", "-712")
                .queryParam( "pdztpopazuys", "")
                .queryParam( "nullable", "width 0 hwjvqge b^*MZt")
                .queryParam( "width", "")
                .queryParam( "j", "234")
                .queryParam( "ibeqgkgqozasj", "-1006")
                .queryParam( "nhptig", ":uH$clo")
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
                .queryParam( "deep[bgkw]", ",&b\",PJ5Y")
                .queryParam( "deep[owslsaidqxpktk]", "1018.5")
                .queryParam( "deep[rixloxgccwc]", "170.3")
                .queryParam( "height", "0")
                .queryParam( "nkc", "true")
                .queryParam( "ltg", "v2|**b,;A`")
                .queryParam( "jzwvmdik", "279")
                .queryParam( "nullable", "width 0 zbsedg 9\"@c")
                .queryParam( "width", "-1")
                .queryParam( "ycswmddrpj", "eck,,pqphgkdy,,wr2B*")
                .queryParam( "mbe", "")
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
                .queryParam( "deep[czzmijdktv]", "")
                .queryParam( "deep[eb]", "\\nk\"CS$")
                .queryParam( "height", "0")
                .queryParam( "crkzgyfjm", "buejgiuniu,439,tulktsc,true,prcvmgrsnvx,189.9")
                .queryParam( "tqnisxfs", "true")
                .queryParam( "dkrvoiecrhwxbrd", "woztzl,|G!?Sdk,sulzzidxaqdijgn,true")
                .queryParam( "nullable", "width 0 fdwpilzapwqixh Asd0! wyw QQOP vy true")
                .queryParam( "height", "768.6")
                .queryParam( "ipfjtqdlq", "-795.9")
                .queryParam( "p", "jfwygeakydk,-388.1,wmeegmm,,latbc,uUr%$")
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
                .queryParam( "deep[vptyy]", "idewwwugnvq,899,qoau,229,idazmqxovjwtjxe,h,OgR!W,v.`N")
                .queryParam( "height", "0")
                .queryParam( "gdurcoojofunpaoo", "true")
                .queryParam( "nullable", "width 0 flfcrwdhxnlsnjl henzdpatw,S,,imuhevdlcsxsnhck,H5_JBR3,k,325")
                .queryParam( "height", "-1")
                .queryParam( "oqftoc", "")
                .queryParam( "mcocxpokwwd", "true")
                .queryParam( "tjye", "")
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
                .queryParam( "deep[cwgcnjfh]", "-1020.2")
                .queryParam( "deep[xtldifqhb]", "true")
                .queryParam( "deep[pudmbuflww]", "A)K@1r7")
                .queryParam( "height", "0")
                .queryParam( "ziapexkrix", "18")
                .queryParam( "qqdz", "")
                .queryParam( "uangtzyrqtexnxv", "R^Lkg&8<")
                .queryParam( "ofpjk", "229")
                .queryParam( "xnrzricrehcny", "-538.9")
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
                .queryParam( "deep[xkmlsjf]", "true")
                .queryParam( "height", "0")
                .queryParam( "gkulweyndok", "")
                .queryParam( "x", "")
                .queryParam( "nullable", "true")
                .queryParam( "vlrxfgfjal", "/#r#")
                .queryParam( "altgzpdylsjbvhee", "true")
                .queryParam( "pxtklnyj", "eszarqwxpfem,)HF9%,) `,R$pYUUg,rtncha,s")
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
                .queryParam( "deep[ayphylsoglbsndcw]", "true")
                .queryParam( "deep[rox]", "ffqfzhwbnfjnidbl,250.4")
                .queryParam( "height", "0")
                .queryParam( "zxdgfenkulr", "true")
                .queryParam( "ueonjtcq", "40")
                .queryParam( "ggkkodphp", "334")
                .queryParam( "nullable", "width true ezbp -2")
                .queryParam( "oetlb", " 2#jtKv3")
                .queryParam( "tdtqnuxihciym", "-331.1")
                .queryParam( "jdc", "751")
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
                .queryParam( "deep[wuqgrpcynzhkzvzz]", "")
                .queryParam( "deep[pju]", "585.8")
                .queryParam( "deep[zhni]", "true")
                .queryParam( "height", "0")
                .queryParam( "dbyaugqyhtsxlhpx", "226.4")
                .queryParam( "lytlopqgyk", "-192.5")
                .queryParam( "nullable", "width -1 jgssf sxvc,298,vsifqsb,186 abklm ^2/{E^\\z")
                .queryParam( "mguqadcsc", "vnrzighs,130,mqptpekalpfp,724.0")
                .queryParam( "xz", "-536.3")
                .queryParam( "dfbfg", "9({jKgZp,V")
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
                .queryParam( "deep[jkuskuf]", "lbwlcckvpbcqqpyv,-587")
                .queryParam( "deep[iocoxraxfilbt]", "689")
                .queryParam( "height", "0")
                .queryParam( "xvstucylnjmuq", "Y}@m")
                .queryParam( "nullable", "width 0 height D'iUY$ jdwlywhsf true aoqafpyewpu -288.3 gxul -1007.5")
                .queryParam( "tbgdvoal", "e|g{+$g8")
                .queryParam( "ignxwmx", "-835")
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
                .queryParam( "deep[xferakkwtbepbp]", "")
                .queryParam( "deep[xyotq]", "")
                .queryParam( "deep[wxnvhwgvdqn]", "")
                .queryParam( "height", "0")
                .queryParam( "wfzriwntbcwxbo", "-152.9")
                .queryParam( "nullable", "width 0 height -1 p 0'3OjpG,qxwpR,6Aq<Z[")
                .queryParam( "miokjjmqfnjwwoy", "65")
                .queryParam( "oeptmghzae", "c}@")
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
                .queryParam( "deep[mkfi]", "-178")
                .queryParam( "nullable", "width 0 vf dhy,661,wglyi,438,xsucsxjatsv,-727 vtos 337 qwgzkwbyj -504.0")
                .queryParam( "icjgntfabgebuj", "cs'0")
                .queryParam( "ldjobnzosoa", "hhatvickgve,0<\"zk4a,ulikgleofcbzy,)")
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
                .queryParam( "deep[ywzclfosyjclvdx]", "-471.5")
                .queryParam( "deep[tiynbvytefdjufr]", "ywoqicjmpho,-734.0,utzxxlrjlr,653.7,fmikjohxwf,916")
                .queryParam( "deep[vqfqfhmcx]", "true")
                .queryParam( "exploded", (String) null)
                .queryParam( "nullable", "width 0 odh -231 uyc 82 t mkkokcjfvfkhcfz,true")
                .queryParam( "xi", "")
                .queryParam( "fwlz", "true")
                .queryParam( "fknvtpfxnhhbbux", "")
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
                .queryParam( "deep[xrzqjy]", "yjk,")
                .queryParam( "exploded", "%l=")
                .queryParam( "nullable", "width 0 bjfsgtvxdwifztl -106 fxitohma $&T%/ uxksezkeucwzhls -726")
                .queryParam( "qwvbeuqgasin", "610.5")
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
                .queryParam( "deep[dzbzrtctmargmb]", "-933")
                .queryParam( "deep[ivcjcvltnrkjyz]", "-680.5")
                .queryParam( "width", "_nS0")
                .queryParam( "height", "0")
                .queryParam( "izf", "-154")
                .queryParam( "qzspqtwhgbmb", "-111.5")
                .queryParam( "sxhs", "e'2BsD8")
                .queryParam( "nullable", "width 0 sjuraetsuzukjb true tczkygjjjyggkeit -579 lomj -138")
                .queryParam( "kqnyod", "")
                .queryParam( "wxvksaeroygxweg", "-314.2")
                .queryParam( "kzonvvpeqmisdaay", "-775")
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
                .queryParam( "deep[zojduuth]", "true")
                .queryParam( "deep[akwkxfporloplup]", "")
                .queryParam( "deep[jlwryyhqnzfzj]", "-189.1")
                .queryParam( "width", "-1")
                .queryParam( "height", "0")
                .queryParam( "iaspjuji", "17.2")
                .queryParam( "nullable", "width 0 uvkuusefiz irvhkpxenm,;,8Nu,a7K,qktmzephlrvvwv,A$ZiUk,D gjpkesg 509.3")
                .queryParam( "xltwirtiprozflz", "")
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
                .queryParam( "deep[ctbawgnkdop]", "kxhadeczytzt,Vy,irj,-681.4")
                .queryParam( "deep[pisudaur]", "-23")
                .queryParam( "height", "h,y,fxmeignim,-228,hsdedql,<#")
                .queryParam( "a", "ntk,R\"T0QQ")
                .queryParam( "rinyxtmiapqafem", "L,rRjuU")
                .queryParam( "nullable", "width 0 wbhbvticw true vtqkfpptiwgbj -635")
                .queryParam( "eroijzbv", "nlnybojgjpwnutqi,C$|^b0,9*k,]s,lsgqjvmwwuxund,true")
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
                .queryParam( "deep[phy]", "-629")
                .queryParam( "deep[azagrxji]", ",D\"MtD9{0")
                .queryParam( "deep[mlbkoyvzbqqm]", "mpdsrxlrn,>U,~BIy~-,zbbreu,,`'$")
                .queryParam( "height", "-1")
                .queryParam( "ciwacmcihlbcqlmv", "true")
                .queryParam( "enksizi", "6.0")
                .queryParam( "qzkrvirbf", "-259")
                .queryParam( "nullable", "width 0 xwyfg true")
                .queryParam( "chczxgpl", "true")
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
                .queryParam( "pkumyiyghya", "true")
                .queryParam( "wdbhd", "~2q8hG\\\\")
                .queryParam( "dkhqtwittvd", "kX")
                .queryParam( "nullable", "width 0 jbduvngzxmwhsy true wxe km,51,ojhnwvufwbvrrf,e~^O:=o,fjljka,utqJ~W0")
                .queryParam( "qqaiellerzxmcnr", "")
                .queryParam( "zxoigxgzo", "")
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
                .queryParam( "meuiiiqwqkjiqg", "414.9")
                .queryParam( "nullable", "width 0 k 816.4")
                .queryParam( "ggwezrmvbbipy", "mxndign,-264,xgrxd,")
                .queryParam( "y", "true")
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
                .queryParam( "deep", "557.3")
                .queryParam( "height", "0")
                .queryParam( "etzr", "true")
                .queryParam( "hclbppuz", "A;&m(gou")
                .queryParam( "qh", "true")
                .queryParam( "nullable", "width 0 yui 3`\"aLF[,d qpi true")
                .queryParam( "zdlflwwlx", "true")
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
                .queryParam( "deep[width]", "DuTz")
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[ht]", "")
                .queryParam( "deep[xqlaxoiusl]", "")
                .queryParam( "height", "0")
                .queryParam( "qhmv", "xttajlwtl,1013,rnojzhyoopp,-290")
                .queryParam( "hgxhxzxyyzfltf", "PyRbF0y;")
                .queryParam( "xrxensq", "142.1")
                .queryParam( "nullable", "width 0 gko /aGcX5| k 324")
                .queryParam( "sqhqjwuxlturde", "yasbclcw,,0o,nkjboziafkgtisnt,true")
                .queryParam( "exreqlofben", "-1016")
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
                .queryParam( "deep[ffmmwqyojrwk]", "gugbvkmj,true,eliu,")
                .queryParam( "height", "0")
                .queryParam( "vta", "")
                .queryParam( "nullable", "width 0 mevrgrmkxbaibef true jwiitfqrgdfnd 132 waclkfssmyoj ")
                .queryParam( "ajzyrpkxa", "-554")
                .queryParam( "gczycuoxjqqgttio", "517.1")
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
                .queryParam( "deep[height]", "")
                .queryParam( "deep[cdnmrbfszn]", "")
                .queryParam( "height", "0")
                .queryParam( "jqdqk", "true")
                .queryParam( "caytupxrzagh", "j7JW,P1u',")
                .queryParam( "nullable", "width 0 kinwtl fopjzdcm,true")
                .queryParam( "rurawhchgwjrik", "410")
                .queryParam( "pjwjv", "^eDrJ6R")
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
                .queryParam( "deep[jkhg]", "267.6")
                .queryParam( "height", "0")
                .queryParam( "lycqnot", "true")
                .queryParam( "mhzqv", "_*vfd")
                .queryParam( "nullable", "width 0 vmo  jbmaggtbu @fVn")
                .queryParam( "q", "122.8")
                .queryParam( "xkl", "true")
                .queryParam( "xmqfhnly", "-891")
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
                .queryParam( "nullable", "S0)$yhhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?")
                .queryParam( "nonEmpty", "5^H")
                .queryParam( "empty", "i.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wzx6{<h}W\"Sk}%-Ll/f10( t^]}<M[_JfU29nXt{k]_oNvmA\\*|\\_GMhY&TgKd[m/3[Xi2s[@b'^1)jCHcY=x\\gDBQyHj[tB6)2c<2,{r[wp4k9sI;5(@jgGr%5`+=w*aG)@b&1avN2IsK#EWAwleN\"]Y,9fw7?5 KJYu$s]/HfvwpDFT Dsdo=T.~p4;!D6|${Go|hn\";j^)1=$}o%zg\\bD`dep5Ftn")
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
                .queryParam( "nonEmpty", "\":SD4:uw,pGY`UWA9GqrV'Au!L.sq*Pf%z&b?lK{2hFEDwG[")
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
                .queryParam( "nonEmpty", "AcS")
                .queryParam( "empty", "W%M'|y3{f5xCL8-Cu|k*Ia9A+*A,|L?-G=V]L8CYg t<P]#{k%AlU1l`p?H|Q~4%@~[NgZ> 9M3>cv.$m]f7qH[r3.GC#rU f7R#?y[aaJ#Y:%`$s(A{@")
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
                .queryParam( "nullable", "@s<\"M5rH[vC?>7(^zKpUPb{ CO2qR}\\Dk iFXrX\"XH&hN'7dl-FE7F$i?^G*Q\\N x*:/3GOUKk5L\"y^5as@3O-;]90Q2kNWES$tnaIbbEvzR;]Y#'(0I4{Lr>ddf523Cj{(#ur5@!]i@lNx2T>[ T.n+4e/QXQFoMTvrLW#X}bu (P,&tLxMMRrgg;PU{$X/&13;Oq9B]BAEm+ET;&8pY>.SkrSOH<zOh.IS")
                .queryParam( "nonEmpty", "l!.*/Y@S}4zRlE7O]^Oj;R(hB:\\#EOdZX@_#9h>Sx*]@2vCg\\z\"N`DZ<):0'yx-G\\$L MJd0T$mTU|Te.j./Z~wa[oW79@!}GdOxSX4_HQoZsW F4j#G(9$4Z9gxd+p>N0dO\\9hw3dRq9;79pOa~ZqI^em4Hd@2*2WW}8ow RJ=\"8Uu^|B}T\\<*e}$Yl!&( 3hQ.G3P{C_2`OKs914cJYO&=(T>{hfh'##I/*uigZ6GmM&Pm%` %DKf!jA,")
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
                .queryParam( "nullable", "\\(DkCa \"+#%nm,B5L3@': xQ`YeX<nq9zrZ\\>W/i'FFp3[cDuks&|t5vCKQjPAF_@KB&HOD7y]8s#C'wGb{A1k?w(RX{T1Jrw#^pfc'V[nyu0(uBY\"upE!j`fgJ${JXIpa67M2i@.lwX")
                .queryParam( "nonEmpty", "IZjb%WWDG:>2AvL#|L1:8E;u#>Dmy:o27HkDZ%rt)listU>?]sAviyah`46\"yhpb\"~'tm1ZV>& S{DOg6(3Wtz>n1bF%j|UIqx[@`~t0Yub'x>\"&w<b|xQ#,:t\"S`9T\\[450nSg")
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
                .queryParam( "nullable", "`.4{wREEbl'H<q%|EdyDlPp`z<rpJWEwMgaqlhIs6/-b9X9sg~x>>tB%,/k~ c\"V_Ak1T=Jco~ D\\X2>_qzE")
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
                .queryParam( "nullable", "@c%")
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
                .queryParam( "nullable", "V@<p idGE`zGK-[#'o/K*#@mPxlw4ejJJBF<m?msiX,SRO\\FRR`~6E'JU(8+l\"o! G-M3VX_UxHz~rHo")
                .queryParam( "nonEmpty", "&3")
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
                .queryParam( "nonEmpty", "shX&U{Bu9J_GI'ekwfY})Fo{JuFa}]]Sd8$&x9_%Bh,Bu3p}9%{Q,2e>=R/-)D`0Q;@f_~3+2Dys=ohR+{T8*hW$:%2Z7Udz)VMKq)8$Zc\\DOqfvFSDLh@^8Bw")
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
