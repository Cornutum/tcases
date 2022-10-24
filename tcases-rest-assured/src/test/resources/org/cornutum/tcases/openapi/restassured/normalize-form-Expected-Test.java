package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import java.util.Map;
import static java.util.stream.Collectors.toMap;

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
                .cookie( "nullable", "")
                .cookie( "exploded", "")
                .cookie( "nonEmpty", "-1022867227")
                .cookie( "nonEmpty", "-1061495024")
                .cookie( "nonEmpty", "-173246836")
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
                .cookie( "nullable", null)
                .cookie( "exploded", "-400734487")
                .cookie( "nonEmpty", null)
                .cookie( "nonEmpty", "-196049713")
                .cookie( "nonEmpty", "-613588290")
                .cookie( "nonEmpty", "971240443")
                .cookie( "nonEmpty", null)
                .cookie( "empty", "-482314267")
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
                .cookie( "nullable", "-193206107")
                .cookie( "exploded", null)
                .cookie( "exploded", "-447883825")
                .cookie( "exploded", "521580942")
                .cookie( "exploded", "507789246")
                .cookie( "exploded", "1066510881")
                .cookie( "exploded", "-373255640")
                .cookie( "exploded", "-248335100")
                .cookie( "exploded", "-979291408")
                .cookie( "exploded", "-412486529")
                .cookie( "exploded", "166110435")
                .cookie( "nonEmpty", "0")
                .cookie( "nonEmpty", "-12499080")
                .cookie( "nonEmpty", "297804974")
                .cookie( "empty", null)
                .cookie( "empty", "599765204")
                .cookie( "empty", "-307439264")
                .cookie( "empty", "-589896441")
                .cookie( "empty", "-286206794")
                .cookie( "empty", "518283748")
                .cookie( "empty", "-688602949")
                .cookie( "empty", "-747189313")
                .cookie( "empty", "882370206")
                .cookie( "empty", "-698276423")
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
                .cookie( "nullable", null)
                .cookie( "nullable", "842603018")
                .cookie( "nullable", "698364951")
                .cookie( "nullable", "234587396")
                .cookie( "nullable", "993898969")
                .cookie( "nullable", "409828839")
                .cookie( "nullable", "72997907")
                .cookie( "nullable", "916948838")
                .cookie( "nullable", "969603412")
                .cookie( "nullable", "837234967")
                .cookie( "nullable", "465617296")
                .cookie( "exploded", "")
                .cookie( "nonEmpty", "21440278")
                .cookie( "nonEmpty", "621989306")
                .cookie( "nonEmpty", "-306025178")
                .cookie( "nonEmpty", "-899374955")
                .cookie( "nonEmpty", "233047813")
                .cookie( "nonEmpty", "482775684")
                .cookie( "nonEmpty", "464467392")
                .cookie( "nonEmpty", "-812088994")
                .cookie( "nonEmpty", "999627450")
                .cookie( "nonEmpty", "-396332896")
                .cookie( "nonEmpty", "-395312537")
                .cookie( "nonEmpty", "233047813")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "0")
                .cookie( "nonEmpty", "-81979590")
                .cookie( "nonEmpty", "-689189967")
                .cookie( "nonEmpty", "-985697460")
                .cookie( "empty", "950501242")
                .cookie( "empty", "441316532")
                .cookie( "empty", "-185235789")
                .cookie( "empty", "573428563")
                .cookie( "empty", "797820610")
                .cookie( "empty", "790111441")
                .cookie( "empty", "-675903017")
                .cookie( "empty", "362324763")
                .cookie( "empty", "573428563")
                .cookie( "empty", "-975501527")
                .cookie( "empty", "633031252")
                .cookie( "empty", "-337731314")
                .cookie( "empty", "978375565")
                .cookie( "empty", "425646374")
                .cookie( "empty", "-809879512")
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
                .cookie( "nullable", "0")
                .cookie( "exploded", "493402000")
                .cookie( "exploded", "-501370975")
                .cookie( "exploded", "895446748")
                .cookie( "exploded", "895446748")
                .cookie( "exploded", "-766742771")
                .cookie( "nonEmpty", "0")
                .cookie( "nonEmpty", "569127818")
                .cookie( "nonEmpty", "307954793")
                .cookie( "nonEmpty", "628890499")
                .cookie( "nonEmpty", "586503469")
                .cookie( "nonEmpty", "-225946473")
                .cookie( "nonEmpty", "695341087")
                .cookie( "nonEmpty", "777948200")
                .cookie( "nonEmpty", "-848695887")
                .cookie( "nonEmpty", "-117691697")
                .cookie( "nonEmpty", "-848695887")
                .cookie( "nonEmpty", "-375700589")
                .cookie( "nonEmpty", "602160028")
                .cookie( "nonEmpty", "-848893352")
                .cookie( "nonEmpty", "88110207")
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
                .cookie( "nullable", "272860764")
                .cookie( "nullable", "-1045675214")
                .cookie( "nullable", "-1045675214")
                .cookie( "exploded", "")
                .cookie( "nonEmpty", "319741859")
                .cookie( "nonEmpty", "549989277")
                .cookie( "nonEmpty", "-503371834")
                .cookie( "empty", "-626245989")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-749905517")
                .cookie( "nonEmpty", "-37219370")
                .cookie( "nonEmpty", "-1029411827")
                .cookie( "nonEmpty", "872427438")
                .cookie( "nonEmpty", "-1070273829")
                .cookie( "nonEmpty", "314208182")
                .cookie( "nonEmpty", "195413426")
                .cookie( "nonEmpty", "1023792597")
                .cookie( "nonEmpty", "247011149")
                .cookie( "nonEmpty", "-611500928")
                .cookie( "nonEmpty", "-996950067")
                .cookie( "nonEmpty", "797540652")
                .cookie( "nonEmpty", "-37219370")
                .cookie( "nonEmpty", "-412388374")
                .cookie( "nonEmpty", "-656236288")
                .cookie( "nonEmpty", "273245006")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-746216280")
                .cookie( "nonEmpty", "-400578368")
                .cookie( "nonEmpty", "683687829")
                .cookie( "nonEmpty", "754362582")
                .cookie( "nonEmpty", "-578773565")
                .cookie( "nonEmpty", "-175959517")
                .cookie( "nonEmpty", "-417674636")
                .cookie( "nonEmpty", "-264207896")
                .cookie( "nonEmpty", "-938493917")
                .cookie( "nonEmpty", "-1032327764")
                .cookie( "nonEmpty", "-218031410")
                .cookie( "nonEmpty", "-97735228")
                .cookie( "nonEmpty", "-1032327764")
                .cookie( "nonEmpty", "941211824")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-558730338")
                .cookie( "nonEmpty", "-539851571")
                .cookie( "nonEmpty", "-67926575")
                .cookie( "nonEmpty", "917875828")
                .cookie( "nonEmpty", "97694981")
                .cookie( "nonEmpty", "-152451859")
                .cookie( "nonEmpty", "-612405954")
                .cookie( "nonEmpty", "-152451859")
                .cookie( "nonEmpty", "-1053665150")
                .cookie( "nonEmpty", "-840681944")
                .cookie( "nonEmpty", "-882409023")
                .cookie( "nonEmpty", "-531066697")
                .cookie( "nonEmpty", "798008823")
                .cookie( "nonEmpty", "-175110027")
                .cookie( "empty", "-107.3")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-817929597")
                .cookie( "nonEmpty", "-79577225")
                .cookie( "nonEmpty", "-2060381")
                .cookie( "nonEmpty", "27363711")
                .cookie( "nonEmpty", "-381533979")
                .cookie( "nonEmpty", "-381533979")
                .cookie( "nonEmpty", "234038641")
                .cookie( "nonEmpty", "324728726")
                .cookie( "nonEmpty", "887848941")
                .cookie( "empty", "true")
                .cookie( "empty", "-569599889")
                .cookie( "empty", "-531782759")
                .cookie( "empty", "-30705755")
                .cookie( "empty", "-833725623")
                .cookie( "empty", "287328703")
                .cookie( "empty", "242552968")
                .cookie( "empty", "281595972")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-401952577")
                .cookie( "empty", "0")
                .cookie( "empty", "-490425104")
                .cookie( "empty", "-24516461")
                .cookie( "empty", "196861552")
                .cookie( "empty", "678735065")
                .cookie( "empty", "645148056")
                .cookie( "empty", "626270488")
                .cookie( "empty", "153794163")
                .cookie( "empty", "143029446")
                .cookie( "empty", "-656005477")
                .cookie( "empty", "-385940811")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-499214232")
                .cookie( "nonEmpty", null)
                .cookie( "empty", "0")
                .cookie( "empty", "-288827136")
                .cookie( "empty", "452597050")
                .cookie( "empty", "-705103453")
                .cookie( "empty", "634224609")
                .cookie( "empty", "1045596152")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-435663594")
                .cookie( "nonEmpty", "-191")
                .cookie( "empty", "0")
                .cookie( "empty", "469721636")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-532921543")
                .cookie( "nonEmpty", "-233278907")
                .cookie( "nonEmpty", "-233278907")
                .cookie( "empty", "0")
                .cookie( "empty", "261503934")
                .cookie( "empty", "862252385")
                .cookie( "empty", "698613707")
                .cookie( "empty", "-719134563")
                .cookie( "empty", "-807773726")
                .cookie( "empty", "-807388763")
                .cookie( "empty", "520509365")
                .cookie( "empty", "-432363695")
                .cookie( "empty", "776416648")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-421331762")
                .cookie( "nonEmpty", "lh,710,fznymusyg,,xcfu,-756")
                .cookie( "nonEmpty", "107021489")
                .cookie( "nonEmpty", "-254192507")
                .cookie( "nonEmpty", "61567040")
                .cookie( "nonEmpty", "225761933")
                .cookie( "nonEmpty", "168047323")
                .cookie( "nonEmpty", "686468579")
                .cookie( "nonEmpty", "301410055")
                .cookie( "nonEmpty", "-714678018")
                .cookie( "nonEmpty", "-254192507")
                .cookie( "nonEmpty", "377889682")
                .cookie( "nonEmpty", "866545315")
                .cookie( "nonEmpty", "1022113008")
                .cookie( "nonEmpty", "751221729")
                .cookie( "nonEmpty", "918293336")
                .cookie( "empty", "0")
                .cookie( "empty", "713378407")
                .cookie( "empty", "803531087")
                .cookie( "empty", "19765985")
                .cookie( "empty", "-961064705")
                .cookie( "empty", "995433338")
                .cookie( "empty", "619757847")
                .cookie( "empty", "-16414414")
                .cookie( "empty", "-364464825")
                .cookie( "empty", "-860858143")
                .cookie( "empty", "423212935")
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
                .cookie( "exploded", "-37260227")
                .cookie( "nonEmpty", "-345301714")
                .cookie( "nonEmpty", "122843982")
                .cookie( "nonEmpty", "-390883234")
                .cookie( "nonEmpty", "-733445622")
                .cookie( "nonEmpty", "-761822090")
                .cookie( "nonEmpty", "-733445622")
                .cookie( "nonEmpty", "843327802")
                .cookie( "nonEmpty", "320527939")
                .cookie( "nonEmpty", "-137168828")
                .cookie( "empty", "0")
                .cookie( "empty", "-573829088")
                .cookie( "empty", "998879584")
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
                .cookie( "nullable", "true")
                .cookie( "exploded", "-35015723")
                .cookie( "nonEmpty", "-119013854")
                .cookie( "nonEmpty", "-159846939")
                .cookie( "nonEmpty", "-48801102")
                .cookie( "nonEmpty", "777827239")
                .cookie( "nonEmpty", "839246861")
                .cookie( "nonEmpty", "-945006533")
                .cookie( "nonEmpty", "-1055658149")
                .cookie( "nonEmpty", "495976937")
                .cookie( "nonEmpty", "-1055658149")
                .cookie( "nonEmpty", "-722904600")
                .cookie( "nonEmpty", "-470897307")
                .cookie( "nonEmpty", "893364956")
                .cookie( "nonEmpty", "-751702821")
                .cookie( "empty", "0")
                .cookie( "empty", "-782178473")
                .cookie( "empty", "464163471")
                .cookie( "empty", "671843785")
                .cookie( "empty", "-944280359")
                .cookie( "empty", "-1066116844")
                .cookie( "empty", "-292165595")
                .cookie( "empty", "552070153")
                .cookie( "empty", "-309023568")
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
                .cookie( "nullable", "7q|&Ss1D")
                .cookie( "exploded", "-886359670")
                .cookie( "nonEmpty", "-842846500")
                .cookie( "nonEmpty", "-530180019")
                .cookie( "nonEmpty", "467733374")
                .cookie( "nonEmpty", "489182817")
                .cookie( "nonEmpty", "-615511957")
                .cookie( "nonEmpty", "975274344")
                .cookie( "nonEmpty", "978123814")
                .cookie( "nonEmpty", "853389147")
                .cookie( "nonEmpty", "853389147")
                .cookie( "nonEmpty", "79535351")
                .cookie( "empty", "0")
                .cookie( "empty", "-904480932")
                .cookie( "empty", "-897384487")
                .cookie( "empty", "605778882")
                .cookie( "empty", "467255264")
                .cookie( "empty", "-639952078")
                .cookie( "empty", "-586041381")
                .cookie( "empty", "-423752356")
                .cookie( "empty", "-985337736")
                .cookie( "empty", "867411905")
                .cookie( "empty", "-871982863")
                .cookie( "empty", "1014119864")
                .cookie( "empty", "211022859")
                .cookie( "empty", "-43901811")
                .cookie( "empty", "743272065")
                .cookie( "empty", "559512243")
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
                .cookie( "nonEmpty", "-202945527")
                .cookie( "nonEmpty", "-202945527")
                .cookie( "nonEmpty", "-849521279")
                .cookie( "nonEmpty", "-886238122")
                .cookie( "empty", "0")
                .cookie( "empty", "-399126641")
                .cookie( "empty", "965653909")
                .cookie( "empty", "437826995")
                .cookie( "empty", "708620537")
                .cookie( "empty", "-823779496")
                .cookie( "empty", "-75585735")
                .cookie( "empty", "355326589")
                .cookie( "empty", "-453655170")
                .cookie( "empty", "755327356")
                .cookie( "empty", "-938386951")
                .cookie( "empty", "431706345")
                .cookie( "empty", "903697015")
                .cookie( "empty", "-578604288")
                .cookie( "empty", "-706478910")
                .cookie( "empty", "-616271751")
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
                .cookie( "nullable", "")
                .cookie( "exploded", null)
                .cookie( "nonEmpty", "-239516131")
                .cookie( "nonEmpty", "102934332")
                .cookie( "nonEmpty", "58672923")
                .cookie( "nonEmpty", "58672923")
                .cookie( "nonEmpty", "975855063")
                .cookie( "nonEmpty", "-237119909")
                .cookie( "nonEmpty", "540921684")
                .cookie( "empty", "0")
                .cookie( "empty", "-801366264")
                .cookie( "empty", "649923840")
                .cookie( "empty", "307451968")
                .cookie( "empty", "-436424658")
                .cookie( "empty", "820410600")
                .cookie( "empty", "256259908")
                .cookie( "empty", "-1030691799")
                .cookie( "empty", "-672055249")
                .cookie( "empty", "-315700616")
                .cookie( "empty", "-1060843127")
                .cookie( "empty", "-218290759")
                .cookie( "empty", "-891462539")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "-982.0")
                .cookie( "nonEmpty", "-191611522")
                .cookie( "nonEmpty", "917862296")
                .cookie( "nonEmpty", "-1040277991")
                .cookie( "nonEmpty", "-993856579")
                .cookie( "nonEmpty", "-709060801")
                .cookie( "nonEmpty", "872486322")
                .cookie( "nonEmpty", "600068449")
                .cookie( "nonEmpty", "-737275130")
                .cookie( "nonEmpty", "-51978542")
                .cookie( "nonEmpty", "-780175919")
                .cookie( "nonEmpty", "-51978542")
                .cookie( "nonEmpty", "-706310875")
                .cookie( "nonEmpty", "906788107")
                .cookie( "nonEmpty", "506784008")
                .cookie( "empty", "0")
                .cookie( "empty", "755703004")
                .cookie( "empty", "-662526930")
                .cookie( "empty", "-546455904")
                .cookie( "empty", "469343781")
                .cookie( "empty", "793475688")
                .cookie( "empty", "416707853")
                .cookie( "empty", "682658170")
                .cookie( "empty", "-185354391")
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
                .cookie( "nullable", "")
                .cookie( "exploded", "true")
                .cookie( "nonEmpty", "-468130513")
                .cookie( "nonEmpty", "-38857415")
                .cookie( "nonEmpty", "-734851406")
                .cookie( "nonEmpty", "-399496876")
                .cookie( "nonEmpty", "1063620451")
                .cookie( "nonEmpty", "-840864500")
                .cookie( "nonEmpty", "707028872")
                .cookie( "nonEmpty", "36059071")
                .cookie( "nonEmpty", "-840864500")
                .cookie( "nonEmpty", "731666143")
                .cookie( "nonEmpty", "561278826")
                .cookie( "empty", "0")
                .cookie( "empty", "-298645823")
                .cookie( "empty", "3435534")
                .cookie( "empty", "78025327")
                .cookie( "empty", "-508902181")
                .cookie( "empty", "139336950")
                .cookie( "empty", "477920761")
                .cookie( "empty", "646201794")
                .cookie( "empty", "685631105")
                .cookie( "empty", "-1035867527")
                .cookie( "empty", "-56624206")
                .cookie( "empty", "-906940222")
                .cookie( "empty", "-841324202")
                .cookie( "empty", "380340446")
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
                .cookie( "deep[wioaxdqwsnrsicu]", "jyhhmqip,-295.1")
                .cookie( "deep[bppgflspcjdijh]", "true")
                .cookie( "nullable", "")
                .cookie( "width", "0")
                .cookie( "height", "0")
                .cookie( "imlwpjfslhyfq", ":y)_c~[*")
                .cookie( "width", "0")
                .cookie( "height", "0")
                .cookie( "fuxdmo", "-580.0")
                .cookie( "drhhynumhzkbkemg", "266.8")
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
                .cookie( "nullable", null)
                .cookie( "width", null)
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
                .cookie( "deep[lvinmiotogyk]", "true")
                .cookie( "nullable", "width|0|height|0|u|-503.7|dauvvfvn|true|pmqxallfy|-446.2")
                .cookie( "height", null)
                .cookie( "cewsqkk", "606.4")
                .cookie( "width", null)
                .cookie( "qqlluaysukmhccc", "sfelclcjsmn,true")
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
                .cookie( "deep[width]", "17134813")
                .cookie( "nullable", "width|")
                .cookie( "width", "235020347")
                .cookie( "width", "226888406")
                .cookie( "height", "836198343")
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
                .cookie( "deep[height]", "418159662")
                .cookie( "deep[hesvrs]", "403")
                .cookie( "deep[cskjdb]", "true")
                .cookie( "nullable", "width|937020040|height||slyojdnhej|iqwuommzamtkjznb,qf&,u,-487.4|tgnbphk|-357.2")
                .cookie( "height", "184078569")
                .cookie( "nibf", "true")
                .cookie( "ghmxeobwogfxxvse", "true")
                .cookie( "xxobvmmgqwxe", "")
                .cookie( "avbtwizqybaqejwt", ":q0/")
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
                .cookie( "nullable", "height|510274143")
                .cookie( "width", "0")
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
                .cookie( "deep[zyyrk]", "slrchkvcdbhfszx,-80,lqfa,-830.9,obe,")
                .cookie( "deep[gljpnlguz]", "n7*U)Y")
                .cookie( "deep[rslbdmsdqef]", "-778.4")
                .cookie( "nullable", "width|0|dpokwlity|true")
                .cookie( "height", "0")
                .cookie( "tvbuhwh", "579")
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
                .cookie( "deep[pevxbuafcb]", "?~wEx!,,<S-L")
                .cookie( "deep[eabmfafywtkim]", "hhvxrm,0u*=&w]',psix,145,a,-420.9")
                .cookie( "deep[mycmvwgijwzhta]", "68.0")
                .cookie( "nullable", "width|0|tjpfuroyrr|znbmuzxgqouwp,true,x,-113,ttvgk,-62|ybdnsc|fpxmsjas,HAh,gpliqgup,ZPs~t$rW,^]$0,vdpjugmgsl,P!rjQ#j.")
                .cookie( "height", "0")
                .cookie( "tdeltjqmnwgjgook", "328.5")
                .cookie( "ezk", "")
                .cookie( "hjaatbjenbuef", "v-.ckg")
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
                .cookie( "deep[lzwieezkm]", ")XE/5w]")
                .cookie( "deep[fuszdj]", "")
                .cookie( "deep[xnsg]", "true")
                .cookie( "nullable", "width|0|asbqifhhltdg|MVyn4vPX")
                .cookie( "height", "0")
                .cookie( "f", "")
                .cookie( "kxlfnwd", "539")
                .cookie( "nonEmpty", "true")
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
                .cookie( "deep[msuhdjkqlhv]", "true")
                .cookie( "deep[ipfjgqhtq]", "-483")
                .cookie( "deep[ilxgi]", "true")
                .cookie( "nullable", "width|0|usgfzumvpsv|308|ilhbeelbmwsugvoo||rzub|161")
                .cookie( "height", "0")
                .cookie( "cbqlty", "264")
                .cookie( "ouclexcaqe", ".Y@")
                .cookie( "qogftmahijmzp", "-873.2")
                .cookie( "width", "sHYFwB+(,SrqQL")
                .cookie( "emnxehr", "-936.4")
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
                .cookie( "deep[wjm]", "-630.4")
                .cookie( "deep[uczknxzlykzutype]", "-984")
                .cookie( "nullable", "width|0|jlsbusaicygxgxwo|-62.9|v|-361")
                .cookie( "height", "0")
                .cookie( "q", "'B(u3,ai[V)1<n,|cq")
                .cookie( "jwova", "CAQHZ|1-,c1,8H")
                .cookie( "uoxrmsfwbrxts", "true")
                .cookie( "width", "-1")
                .cookie( "iauydpvvf", "true")
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
                .cookie( "deep[isfghciepiwkj]", "xhkpkbcb,true")
                .cookie( "deep[nyroo]", "`*ZjLziI")
                .cookie( "deep[rc]", "-275.6")
                .cookie( "nullable", "width|0|ybrobueapf|wzqysvmcb,64.9")
                .cookie( "height", "0")
                .cookie( "biyycr", "-428.6")
                .cookie( "yvozhahkcfckrive", "")
                .cookie( "height", "")
                .cookie( "slr", "292.3")
                .cookie( "llrbyq", "161.3")
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
                .cookie( "deep[nffxfxvqjwvozdc]", "lhfembfzulip,`#,On,%qZ?}#z,oilvlbqawgmkytw,w,mqmgxikxlyidy,^,Wknx9$H")
                .cookie( "nullable", "width|0|hjgesb|jjlx,<3[|nzvt|-598.2|qlguiodj|")
                .cookie( "height", "0")
                .cookie( "xmiuq", "hO3bf?S")
                .cookie( "yboywu", "-272")
                .cookie( "dw", "cvgksowatehiad,,upwybufevdjvifpg,-668,ou,541.3")
                .cookie( "height", "-1")
                .cookie( "juzjqscmjvsmh", "true")
                .cookie( "mvq", "b~(4l5n")
                .cookie( "hszuda", "c{0'")
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
                .cookie( "deep[ou]", "true")
                .cookie( "deep[xke]", "")
                .cookie( "height", "0")
                .cookie( "o", "true")
                .cookie( "cbxulmdrul", "DH[ii")
                .cookie( "gbqzzqmnebhsy", "gawycgw,1t,oxoxtbnivrfrthbv,y,cmn,726")
                .cookie( "pm", "646")
                .cookie( "hkqapk", "$%di")
                .cookie( "uljthfeimgqjvsu", "701")
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
                .cookie( "deep[jjawqcxnhoqdmgwp]", "a,xr,uon,593,kuwspww,@2[v=]2C,SNzbl,|q_u")
                .cookie( "deep[pvkdnn]", "")
                .cookie( "nullable", "810")
                .cookie( "height", "0")
                .cookie( "sl", "-233")
                .cookie( "thj", "")
                .cookie( "nfsyd", "690")
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
                .cookie( "deep[baookrgkpa]", "true")
                .cookie( "nullable", "width|dmmpydaap,650.4|hd|-378.0")
                .cookie( "height", "0")
                .cookie( "lbhl", "t1z!o")
                .cookie( "jrbrzdiqeogno", "true")
                .cookie( "enteol", "true")
                .cookie( "ko", "eT2bGe")
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
                .cookie( "deep[rhcszzagu]", "-752")
                .cookie( "deep[bkazfhsrvfwq]", "")
                .cookie( "nullable", "width|-1|xezbznpdnttnjvxu|pjggu,-391,uz,)&jkwbN,wvvzo,true|qbzlpukdc|-13|osu|949.7")
                .cookie( "height", "0")
                .cookie( "bzfzovbuww", "mtvntxmvjpyenchh,r.S>,1h?")
                .cookie( "lprungy", "%N")
                .cookie( "rfvyponittz", "vnqycgxjgnlllboh,true")
                .cookie( "pr", "7u]?lf,.|}eta")
                .cookie( "yrocuxnauccu", "412")
                .cookie( "ucydnpbyf", "-477")
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
                .cookie( "deep[pm]", "true")
                .cookie( "nullable", "width|0|height|-868.2|vieeucziiknyexor|629.7|klbagbipbouqqih|f")
                .cookie( "height", "0")
                .cookie( "kynjj", "-564")
                .cookie( "ymdqfcitbbmeeofl", "co,1002,jodxuplhuks,true,xbnaczgyjf,true")
                .cookie( "qjs", "ysqjulkkyjsd,")
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
                .cookie( "deep[ulxvpgknbpykwn]", "-943")
                .cookie( "deep[vilq]", "true")
                .cookie( "nullable", "width|0|height|-1|ffbvhbdrsm|Yb?TJu|q|epkssurjtrnsu,53xvI1$,a~n|thamamnhul|596.6")
                .cookie( "height", "0")
                .cookie( "qzgcmypcsct", "true")
                .cookie( "smykgxtgj", "")
                .cookie( "rc", "-7z-e@")
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
                .cookie( "deep[xjxnjyvqmtocpufz]", "pbfmprnigjjp,461,caannwoanhe,0|4_GP,sexdwaeixjt,316")
                .cookie( "nullable", "width|0|skfgz|-865.0|rgbfgtlhbcdycw|yiiyrmsa,true,sdyxuph,723.6,hliqdozpo,")
                .cookie( "mafjindgho", "true")
                .cookie( "zkmaqa", "996")
                .cookie( "rkz", "true")
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
                .cookie( "deep[kvgosvrvhpa]", "I4nG,$iDH1ea,")
                .cookie( "deep[s]", "ttaomjplrzx,405,hbeh,70.5")
                .cookie( "nullable", "width|0|s|-975")
                .cookie( "exploded", null)
                .cookie( "mwppmd", "Q~X@")
                .cookie( "aknahxuuawfks", "tvbenftoxmqdf,NMnz,eseynvlornzsq,>-H")
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
                .cookie( "deep[qlbrgbgohofefqge]", "973.0")
                .cookie( "nullable", "width|0|gt|-577|ecpxlpuhyt||hgcwjjyo|t]U")
                .cookie( "exploded", "166.9")
                .cookie( "cufvgbwuorvln", "917")
                .cookie( "romuloskbxgcgblv", "h")
                .cookie( "ctbq", "")
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
                .cookie( "deep[yaewx]", "-46")
                .cookie( "nullable", "width|0|slnpva|true|decxpfmbbfci|544.7|pbiknsqz|764")
                .cookie( "width", "true")
                .cookie( "height", "0")
                .cookie( "ns", "<K<.&L8!,b'--,")
                .cookie( "lgqoiuuzzy", "U(ndH,A87i}<")
                .cookie( "o", "true")
                .cookie( "woxkwgj", "-187")
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
                .cookie( "deep[yv]", "234")
                .cookie( "nullable", "width|0|agzcpghjwy|732.1|sjgsfthbiisenb|6]|ghxeuaqev|true")
                .cookie( "width", "-1")
                .cookie( "height", "0")
                .cookie( "pqje", "")
                .cookie( "av", "z")
                .cookie( "imxeyee", "true")
                .cookie( "uulygozfw", "AuYtJ,OBJ,:tiKJRI{")
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
                .cookie( "deep[osqw]", "Pb-wI")
                .cookie( "deep[zjssoye]", "442.1")
                .cookie( "nullable", "width|0|bsbqild|-402.2|gtvpatbrk|kkxjjruosd,true,wmuuiuyqifei,],xmazyajj,true")
                .cookie( "height", "1010.6")
                .cookie( "kked", "807")
                .cookie( "jigqvookqyczgd", "oi,647.7,dzhdxgihaftwiy,623,wofyomatsvo,")
                .cookie( "usalbznkwd", "true")
                .cookie( "vld", "_YZ")
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
                .cookie( "deep[jfx]", "-911")
                .cookie( "deep[rmxnfyy]", "true")
                .cookie( "deep[qxrxkncehm]", "j2,o!P{'mk,")
                .cookie( "nullable", "width|0|zblflmaujpcoi|true|rlsgharb|z!JRP4jU")
                .cookie( "height", "-1")
                .cookie( "vbhbeyvn", "true")
                .cookie( "eye", "tdhou,-670.6")
                .cookie( "nlnerilby", "<X?^?")
                .cookie( "ddoaujth", "iplvnpcp,true,ynhuppawuinpmtbi,?F[_'~r,lkeucsgmse,true")
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
                .cookie( "nullable", "width|0|jnysvttvmbd|-370")
                .cookie( "height", "0")
                .cookie( "xnbehpoetrbymcw", "")
                .cookie( "imedjcbchbmtdni", "true")
                .cookie( "vwnwfvcbf", "Nk?")
                .cookie( "tnfu", "lasyotifn,")
                .cookie( "o", "_m")
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
                .cookie( "nullable", "width|0|djnwdno|kF79R|hxopaggucpt|989|oxqqnmotnumji|6H")
                .cookie( "height", "0")
                .cookie( "fkzescku", "3t:Qh',T,d")
                .cookie( "yjaudfplnef", "n)w'")
                .cookie( "xuoxjlcmf", "-730.5")
                .cookie( "qxparuvdijlapvoi", "")
                .cookie( "lidkhysdq", "true")
                .cookie( "cgbpzhriyhkzci", "+z0")
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
                .cookie( "deep", "-653.6")
                .cookie( "nullable", "width|0|luwb|true")
                .cookie( "height", "0")
                .cookie( "rrhulgknya", "")
                .cookie( "yp", "Ob0>ni!")
                .cookie( "ihlaxtvelcvzm", "-8.2")
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
                .cookie( "deep[width]", "MZ,66fp],j#v|'u")
                .cookie( "deep[height]", "0")
                .cookie( "deep[w]", "-295.4")
                .cookie( "nullable", "width|0|clpvtmtfdjgj|dfdpvrws,LM0v|cuo|&AOC")
                .cookie( "height", "0")
                .cookie( "nyv", "true")
                .cookie( "psvrbtyix", "rmj,E")
                .cookie( "awualpmklx", "jydjrgh,209.6,upp,73.8")
                .cookie( "iihji", "}<5|zgE")
                .cookie( "pen", "-424")
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
                .cookie( "deep[vv]", "]&fqN/@}")
                .cookie( "deep[wifv]", "ktxzu,-336.4")
                .cookie( "deep[yjgczpyctcpsgpnc]", "J[zd")
                .cookie( "nullable", "width|0|kpnismixk|zp1)<f>|rsbrb||vqmrlvj|#$dP")
                .cookie( "height", "0")
                .cookie( "xhn", "jpynptjwmevauj,YjyC(,nYfr:z,wulctyvgvlwwudf,,wojzzqlfjju,")
                .cookie( "hwbowwpvdgeedege", "671.6")
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
                .cookie( "deep[height]", "qYy")
                .cookie( "deep[rdugublb]", "`m(o")
                .cookie( "deep[lyonoqbqquk]", "iikwmemwvgv,234.2,ukmfyjxegtvblkgb,L1uW.PE,6<lv:,lskseghfezt,true")
                .cookie( "nullable", "width|0|uhuz|887.5|yrdo|7%a|a|jzcpq,-321")
                .cookie( "height", "0")
                .cookie( "xdfpcimfv", "")
                .cookie( "bdaadwlvmird", "12")
                .cookie( "amaothxmnblf", "-514")
                .cookie( "jnwnmcuhueofq", "-425")
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
                .cookie( "deep[owzyjruvcoubq]", "wgeyoogftky,")
                .cookie( "nullable", "width|0|balkmnioulex|=>,xJG")
                .cookie( "height", "0")
                .cookie( "fughutwrc", "true")
                .cookie( "xllnai", "tcaeekmmemqlfgqj,true,nphr,true,iemgnflnytanjqz,")
                .cookie( "sfxz", "dQ<d,nPtGdj,Z2mI9")
                .cookie( "rfvgvuhaodbr", "!vYe`r,f")
                .cookie( "lqmbzdghpacq", "-302.6")
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
                .cookie( "nullable", "B>w|>up#-nb^bI[?]2nNtU.6`s$Q)6p}Nadv/t9Fl#<*}5>PD]M[f{TYclnf&UyTc+$|&!>kjFKYoGStPT+(x{RC5b?:s%y.Ew4z='vhT<*$713|*eOu]%SG[4*j_.&S+Vf#0olc)*i:CfWyF.k{0u0#do:4LO=_J=cPUURb-e8@a(r_g&%vARlq^ab.M")
                .cookie( "nonEmpty", "xf1")
                .cookie( "empty", "$`UZ")
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
                .cookie( "nonEmpty", "{_R#fUhZ{y}<SL`h<UbdBNgm(2f(S:ODeK^}V&dz/asxNzc!<(yTi1eX@oH")
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
                .cookie( "nonEmpty", "mL2")
                .cookie( "empty", "P(QB}(e*?DT6{P>(3I#u|d%*0R-Er{EI0(X:$#ZfwQ6`BN37")
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
                .cookie( "nullable", "_R$M^RK8=hC[B6t:8wpg/l_/b^0Ho7c1ICX%i@kZAb-4O!WMRR^lV:7)TH+ISlE<P2YnpN5OF7?}?s9Qj}Gq&6n@39(b<otAk`gk-f*c2Jg%zlS+RC)JNL@~uKe^YtI)}:6U_xYCv3=vJr*bj.0pFU#a)-F>pC_Ns+axROFD}-|}_4~{z!ze4yIG8wr:g0#w<zjM}v~=VBFA)cM&=DA|AW8y/4npnzl*dUaMY>6r")
                .cookie( "nonEmpty", "n.0_)!7vd[wd_5F")
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
                .cookie( "nullable", "Oq/]{4a2}/u3h>F$+{K9/.z.HbOek2po*@)J|$<Vu/NW&j^T^OT0c92Pw#mB|.#236M&sp]4^3zo.g0)A5<c9ZBW")
                .cookie( "nonEmpty", "i/w?")
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
                .cookie( "nullable", "6^Z9cHy+DyOY+9gFyw#/5%~<g.kyzz*$:Q~q9(rO^{SJN}V$cCu*ZqWPsO:Af[:GF)_1ctcut_iJoH_Z)j$5(AK(acAMQKLBf3[]'k")
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
                .cookie( "nullable", "ss++uy/%175`6tNy[2TnL@|b+cw]h=)F1reH270/3<HA^I(:GIHQ)IPo*zK#_acf/&(7c9tsY4I_nrL*Q*s*l!2<pfbP+M}Gd.F+=jtY0~1Tm7^>cIEw=tCYbK:&bn<=w><RxZ70Vk=1oV()V(~P(Uy)ypnr0]f4XHTthGYC/UF8P2%!8f5P:4h%VWYTs:!m3F9=9=Kl'+/UZy6V5[%p&zxCb]XhOM")
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
                .cookie( "nullable", "Do%}A2h(R4-lu>$QSd%5|[Z)I&&:'I3ZMsCvVEO6T+%(#5Pnyef6bKd=y@Q'7Ve3z'R>[M~ML61W/mbcGTyR9FD{*#N2goef6zEsXK}|<%kVU2='<0dSmIhN?(0p@'9P7=`z|^0@1-<i_$7F{0OJZMMm67%&|z7}RV]2+EL=`X`d/23{Qqt4")
                .cookie( "nonEmpty", "sK")
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
                .cookie( "nonEmpty", "~c~RH837Iw.rA8Q{_m^JAUB[7c>3|!.UBXs?H{KTVIZGp{XP7^R1:uWWL-")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "")
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
                .queryParam( "nullable", (String) null)
                .queryParam( "exploded", "-409788165")
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
                .queryParam( "nullable", "-236675136")
                .queryParam( "exploded", (String) null)
                .queryParam( "exploded", "1004225667")
                .queryParam( "exploded", "-690938211")
                .queryParam( "exploded", "-138887481")
                .queryParam( "exploded", "-911746819")
                .queryParam( "exploded", "295321005")
                .queryParam( "nonEmpty", "0")
                .queryParam( "nonEmpty", "-89076005")
                .queryParam( "nonEmpty", "800966307")
                .queryParam( "empty", (String) null)
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
                .queryParam( "empty", "123553467")
                .queryParam( "empty", "183027205")
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
                .queryParam( "nullable", (String) null)
                .queryParam( "nullable", "-867763683")
                .queryParam( "nullable", "891562829")
                .queryParam( "nullable", "-311604734")
                .queryParam( "nullable", "25004459")
                .queryParam( "nullable", "440827284")
                .queryParam( "nullable", "643562827")
                .queryParam( "nullable", "549383527")
                .queryParam( "exploded", "")
                .queryParam( "nonEmpty", "103026289")
                .queryParam( "nonEmpty", "-117342421")
                .queryParam( "nonEmpty", "-249592874")
                .queryParam( "nonEmpty", "-654625390")
                .queryParam( "nonEmpty", "-374764058")
                .queryParam( "nonEmpty", "-947054118")
                .queryParam( "nonEmpty", "92548822")
                .queryParam( "nonEmpty", "-147471349")
                .queryParam( "nonEmpty", "-36517737")
                .queryParam( "nonEmpty", "653875233")
                .queryParam( "nonEmpty", "361362064")
                .queryParam( "nonEmpty", "-489511924")
                .queryParam( "nonEmpty", "397261534")
                .queryParam( "nonEmpty", "-489511924")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "0")
                .queryParam( "nonEmpty", "-816934983")
                .queryParam( "nonEmpty", "-153606395")
                .queryParam( "nonEmpty", "335508388")
                .queryParam( "empty", "936666193")
                .queryParam( "empty", "-571813211")
                .queryParam( "empty", "128242613")
                .queryParam( "empty", "-781505315")
                .queryParam( "empty", "-570263001")
                .queryParam( "empty", "-921719413")
                .queryParam( "empty", "121200832")
                .queryParam( "empty", "715649796")
                .queryParam( "empty", "792863197")
                .queryParam( "empty", "-781505315")
                .queryParam( "empty", "882457041")
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
                .queryParam( "nullable", "0")
                .queryParam( "exploded", "967908623")
                .queryParam( "exploded", "-377188700")
                .queryParam( "exploded", "-822201812")
                .queryParam( "exploded", "-994796150")
                .queryParam( "exploded", "78846419")
                .queryParam( "exploded", "-753444122")
                .queryParam( "exploded", "-880047740")
                .queryParam( "exploded", "-567647683")
                .queryParam( "exploded", "-924716132")
                .queryParam( "exploded", "-924716132")
                .queryParam( "nonEmpty", "0")
                .queryParam( "nonEmpty", "368833137")
                .queryParam( "nonEmpty", "-242621399")
                .queryParam( "nonEmpty", "-670425985")
                .queryParam( "nonEmpty", "-93002050")
                .queryParam( "nonEmpty", "415934617")
                .queryParam( "nonEmpty", "126073958")
                .queryParam( "nonEmpty", "-751396076")
                .queryParam( "nonEmpty", "-438495229")
                .queryParam( "nonEmpty", "-423446425")
                .queryParam( "nonEmpty", "368833137")
                .queryParam( "nonEmpty", "-636370163")
                .queryParam( "nonEmpty", "-192573165")
                .queryParam( "nonEmpty", "-950503940")
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
                .queryParam( "nullable", "779116827")
                .queryParam( "nullable", "-687416322")
                .queryParam( "nullable", "-687416322")
                .queryParam( "nullable", "-189516033")
                .queryParam( "nullable", "690235927")
                .queryParam( "nullable", "915333261")
                .queryParam( "nullable", "1001400304")
                .queryParam( "nullable", "57032376")
                .queryParam( "nullable", "-884616211")
                .queryParam( "exploded", "")
                .queryParam( "nonEmpty", "1018882508")
                .queryParam( "nonEmpty", "-674126482")
                .queryParam( "nonEmpty", "-1063310230")
                .queryParam( "empty", "-10032195")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-404501821")
                .queryParam( "nonEmpty", "-330932123")
                .queryParam( "nonEmpty", "-316988181")
                .queryParam( "nonEmpty", "-37708869")
                .queryParam( "nonEmpty", "-486985993")
                .queryParam( "nonEmpty", "604896465")
                .queryParam( "nonEmpty", "-316988181")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-1058884371")
                .queryParam( "nonEmpty", "-271415984")
                .queryParam( "nonEmpty", "-154202023")
                .queryParam( "nonEmpty", "572157103")
                .queryParam( "nonEmpty", "996514317")
                .queryParam( "nonEmpty", "-586465646")
                .queryParam( "nonEmpty", "861814351")
                .queryParam( "nonEmpty", "572157103")
                .queryParam( "nonEmpty", "997795463")
                .queryParam( "nonEmpty", "548205182")
                .queryParam( "nonEmpty", "-955614672")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-519932006")
                .queryParam( "nonEmpty", "-937381090")
                .queryParam( "nonEmpty", "-652828067")
                .queryParam( "nonEmpty", "-44533766")
                .queryParam( "nonEmpty", "-72568761")
                .queryParam( "nonEmpty", "-684674302")
                .queryParam( "nonEmpty", "-673522965")
                .queryParam( "nonEmpty", "195851936")
                .queryParam( "nonEmpty", "-882105637")
                .queryParam( "nonEmpty", "735564063")
                .queryParam( "nonEmpty", "-400822681")
                .queryParam( "nonEmpty", "633376880")
                .queryParam( "nonEmpty", "735564063")
                .queryParam( "nonEmpty", "854231430")
                .queryParam( "empty", "CBg2d\"")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-1071044631")
                .queryParam( "nonEmpty", "-777855890")
                .queryParam( "nonEmpty", "-50042274")
                .queryParam( "nonEmpty", "-998087886")
                .queryParam( "nonEmpty", "268034420")
                .queryParam( "nonEmpty", "801516144")
                .queryParam( "nonEmpty", "1021786249")
                .queryParam( "nonEmpty", "-573290346")
                .queryParam( "nonEmpty", "261012018")
                .queryParam( "nonEmpty", "520338090")
                .queryParam( "nonEmpty", "-891759025")
                .queryParam( "nonEmpty", "481606890")
                .queryParam( "nonEmpty", "577902954")
                .queryParam( "nonEmpty", "-50042274")
                .queryParam( "nonEmpty", "246748580")
                .queryParam( "nonEmpty", "-584902327")
                .queryParam( "empty", "lfasqtdgnsydou,true")
                .queryParam( "empty", "502985810")
                .queryParam( "empty", "432246686")
                .queryParam( "empty", "775003968")
                .queryParam( "empty", "889044240")
                .queryParam( "empty", "44075002")
                .queryParam( "empty", "-1065000929")
                .queryParam( "empty", "-895277541")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-1064624872")
                .queryParam( "empty", "0")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-417805009")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-978724008")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-860553158")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-460973943")
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
                .queryParam( "nullable", "6E'JU(8")
                .queryParam( "exploded", "-647330132")
                .queryParam( "nonEmpty", "-566436264")
                .queryParam( "nonEmpty", "68522676")
                .queryParam( "nonEmpty", "-566436264")
                .queryParam( "nonEmpty", "407064787")
                .queryParam( "nonEmpty", "-120204554")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-949367959")
                .queryParam( "empty", "674203133")
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
                .queryParam( "empty", "739788595")
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
                .queryParam( "nullable", "332.6")
                .queryParam( "exploded", "-426193033")
                .queryParam( "nonEmpty", "-376829912")
                .queryParam( "nonEmpty", "368659127")
                .queryParam( "nonEmpty", "-747842113")
                .queryParam( "nonEmpty", "877640584")
                .queryParam( "nonEmpty", "987236880")
                .queryParam( "nonEmpty", "588154396")
                .queryParam( "nonEmpty", "-694047682")
                .queryParam( "nonEmpty", "934770509")
                .queryParam( "nonEmpty", "588154396")
                .queryParam( "nonEmpty", "-493020903")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-988586485")
                .queryParam( "empty", "-549008269")
                .queryParam( "empty", "-542867155")
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
                .queryParam( "nonEmpty", "-792913942")
                .queryParam( "nonEmpty", "-985128916")
                .queryParam( "nonEmpty", "-723209788")
                .queryParam( "nonEmpty", "-290390595")
                .queryParam( "nonEmpty", "-19283938")
                .queryParam( "nonEmpty", "-19283938")
                .queryParam( "nonEmpty", "847647298")
                .queryParam( "empty", "0")
                .queryParam( "empty", "-275745314")
                .queryParam( "empty", "-699968594")
                .queryParam( "empty", "-954460197")
                .queryParam( "empty", "-562176900")
                .queryParam( "empty", "960723292")
                .queryParam( "empty", "115819170")
                .queryParam( "empty", "488764887")
                .queryParam( "empty", "1037743411")
                .queryParam( "empty", "-616396456")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", (String) null)
                .queryParam( "nonEmpty", "-878797277")
                .queryParam( "nonEmpty", "-701386008")
                .queryParam( "nonEmpty", "-74529999")
                .queryParam( "nonEmpty", "140594522")
                .queryParam( "nonEmpty", "243046145")
                .queryParam( "nonEmpty", "-701386008")
                .queryParam( "nonEmpty", "1053670037")
                .queryParam( "nonEmpty", "-831781577")
                .queryParam( "empty", "0")
                .queryParam( "empty", "523460091")
                .queryParam( "empty", "933318457")
                .queryParam( "empty", "833131836")
                .queryParam( "empty", "-78556733")
                .queryParam( "empty", "908992362")
                .queryParam( "empty", "-443126775")
                .queryParam( "empty", "-433697934")
                .queryParam( "empty", "439911168")
                .queryParam( "empty", "809309101")
                .queryParam( "empty", "-870502987")
                .queryParam( "empty", "931718676")
                .queryParam( "empty", "94289949")
                .queryParam( "empty", "1000117139")
                .queryParam( "empty", "90295147")
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
                .queryParam( "nullable", "")
                .queryParam( "chmsxgaiffxwjdv", "%2Z7Udz),MKq)8$")
                .queryParam( "nonEmpty", "-837203869")
                .queryParam( "nonEmpty", "-1046568058")
                .queryParam( "nonEmpty", "-114984877")
                .queryParam( "nonEmpty", "824744969")
                .queryParam( "nonEmpty", "124911247")
                .queryParam( "nonEmpty", "124911247")
                .queryParam( "nonEmpty", "140565933")
                .queryParam( "nonEmpty", "-325241725")
                .queryParam( "nonEmpty", "-961763562")
                .queryParam( "nonEmpty", "927178423")
                .queryParam( "nonEmpty", "-41576849")
                .queryParam( "nonEmpty", "72410734")
                .queryParam( "empty", "0")
                .queryParam( "empty", "420665591")
                .queryParam( "empty", "78627739")
                .queryParam( "empty", "-925940599")
                .queryParam( "empty", "-989813468")
                .queryParam( "empty", "-452680549")
                .queryParam( "empty", "-111445797")
                .queryParam( "empty", "-282720798")
                .queryParam( "empty", "962555649")
                .queryParam( "empty", "-123731175")
                .queryParam( "empty", "-803554183")
                .queryParam( "empty", "550846493")
                .queryParam( "empty", "-350584145")
                .queryParam( "empty", "536497289")
                .queryParam( "empty", "-576321328")
                .queryParam( "empty", "-624055355")
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
                .queryParam( "nullable", "")
                .queryParam( "exploded", "-508.1")
                .queryParam( "nonEmpty", "-697861720")
                .queryParam( "nonEmpty", "-399027577")
                .queryParam( "nonEmpty", "-558516118")
                .queryParam( "nonEmpty", "624177041")
                .queryParam( "nonEmpty", "-558516118")
                .queryParam( "nonEmpty", "648756300")
                .queryParam( "nonEmpty", "-669106153")
                .queryParam( "nonEmpty", "-129272108")
                .queryParam( "empty", "0")
                .queryParam( "empty", "892919141")
                .queryParam( "empty", "469899679")
                .queryParam( "empty", "490913067")
                .queryParam( "empty", "-846139301")
                .queryParam( "empty", "74017987")
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
                .queryParam( "deep[t]", "true")
                .queryParam( "deep[vmu]", ",,,~6E'{_>")
                .queryParam( "deep[xyr]", "'k")
                .queryParam( "nullable", "")
                .queryParam( "width", "0")
                .queryParam( "height", "0")
                .queryParam( "ibezcj", "^P@xz&}?")
                .queryParam( "width", "0")
                .queryParam( "height", "0")
                .queryParam( "dnztec", "[0E(:bV,")
                .queryParam( "vx", "450")
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
                .queryParam( "nullable", (String) null)
                .queryParam( "width", (String) null)
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
                .queryParam( "deep[vkkomzdvjqqsojnl]", ",\"&0")
                .queryParam( "deep[g]", "-223.5")
                .queryParam( "nullable", "width 0 height 0 awxwxohm  ibvi 484 dxivpimginhnn fdrnomrlex,true")
                .queryParam( "height", (String) null)
                .queryParam( "fsrevwvibggt", "B")
                .queryParam( "qh", "gigmaypvesiyzb,{,kihoxj,460.8")
                .queryParam( "width", (String) null)
                .queryParam( "sqbwalr", "-975")
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
                .queryParam( "deep[width]", "486958295")
                .queryParam( "nullable", "width ")
                .queryParam( "width", "343346692")
                .queryParam( "width", "64986757")
                .queryParam( "height", "336446028")
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
                .queryParam( "deep[height]", "926355998")
                .queryParam( "deep[ossjwswhv]", "\"|_n]elp")
                .queryParam( "deep[vypttqjfrilugl]", "tevovkw,-139")
                .queryParam( "deep[wlggjtgspmbv]", "528")
                .queryParam( "nullable", "width 1057333569 height  sjkgx fHBC\"C uzplzlgihskr 957 mlflu -429.8")
                .queryParam( "height", "669999839")
                .queryParam( "qikx", "Uu?p,D[.7c/\\,~PaT\\:")
                .queryParam( "edqjf", "851")
                .queryParam( "r", "430")
                .queryParam( "qlmatldlbgnmlcfz", "765")
                .queryParam( "bhmjwfdq", "")
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
                .queryParam( "nullable", "height 556362815")
                .queryParam( "width", "0")
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
                .queryParam( "deep[ljsbkwqz]", "")
                .queryParam( "deep[bbptfpytc]", "SVmj8~t")
                .queryParam( "deep[ibptsudv]", "")
                .queryParam( "nullable", "width 0 ztpjurulvrjpy \"3 bco  nznrzwbqxibipkj true")
                .queryParam( "height", "0")
                .queryParam( "drocdtzp", "")
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
                .queryParam( "deep[byrjncljsrfgnsz]", "yyjloqdswuoqm,-819")
                .queryParam( "deep[fj]", "")
                .queryParam( "deep[vfc]", "-161")
                .queryParam( "nullable", "width 0 kpollw 739 oamo ")
                .queryParam( "height", "0")
                .queryParam( "jvsbkmwthgjsioqe", "eimgdokpxsjxige,true,xysmtfokculr,true,vljg,-386.9")
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
                .queryParam( "deep[ykouuvo]", "759")
                .queryParam( "deep[zbspwdlfzdfwar]", "-113.1")
                .queryParam( "nullable", "width 0 pvmsknkcap 721")
                .queryParam( "height", "0")
                .queryParam( "tbcwghyh", "607.9")
                .queryParam( "gadafvlzcginj", "jzjdddncrrqkq,416,nrfnmbv,")
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
                .queryParam( "deep[mbpa]", "-75")
                .queryParam( "deep[ocxlsydelqzdig]", "")
                .queryParam( "deep[efpctii]", "-")
                .queryParam( "nullable", "width 0 nebumaaihxpxoqem 8k3WC^4h,7 dmuiwm =4N mvsmvrqjnvmrb yy'=b~8")
                .queryParam( "height", "0")
                .queryParam( "jegpbzi", "GoE`c@,\\G")
                .queryParam( "width", "true")
                .queryParam( "vtzq", "-790")
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
                .queryParam( "deep[wymip]", "SHA\\.")
                .queryParam( "nullable", "width 0 can true erefsox -914.2")
                .queryParam( "height", "0")
                .queryParam( "uiazicdk", "")
                .queryParam( "o", "t5y_bI&,:q1a ],kTwZ")
                .queryParam( "width", "-1")
                .queryParam( "yyls", "")
                .queryParam( "d", "true")
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
                .queryParam( "deep[tcygp]", "561.5")
                .queryParam( "nullable", "width 0 zckxr ,5]SQ qfoepdeah :,cyRDN; vunc true")
                .queryParam( "height", "0")
                .queryParam( "qzgycgxzgeioyx", "true")
                .queryParam( "height", "hjD7Sa")
                .queryParam( "gdompeyvnwnywh", "true")
                .queryParam( "qmrfxoazl", "qamxkhyeblmbs,a")
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
                .queryParam( "deep[imtsswdininf]", "true")
                .queryParam( "deep[ccbhsjaz]", "149.1")
                .queryParam( "nullable", "width 0 nskmtqnu 751.4")
                .queryParam( "height", "0")
                .queryParam( "pndvehfneedrb", "true")
                .queryParam( "zbskvujys", "-548")
                .queryParam( "grm", "B}7")
                .queryParam( "height", "-1")
                .queryParam( "yjecqide", "true")
                .queryParam( "qiyaxcxny", "864")
                .queryParam( "lfqgrweu", "-845.6")
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
                .queryParam( "deep[oth]", "859")
                .queryParam( "deep[njfjruatb]", "x8mx@")
                .queryParam( "height", "0")
                .queryParam( "qvwpshfzuqj", "true")
                .queryParam( "k", "941")
                .queryParam( "dgdgcqzi", "")
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
                .queryParam( "deep[fnjmkyyitv]", "-889")
                .queryParam( "deep[stte]", "-987.3")
                .queryParam( "nullable", "c:")
                .queryParam( "height", "0")
                .queryParam( "cqlalhxfitjpxter", "qhtih,183.0")
                .queryParam( "snhtf", "true")
                .queryParam( "gzeecniahogihez", "true")
                .queryParam( "xgey", "-674")
                .queryParam( "gzufuiowtvsgzw", "234")
                .queryParam( "cydlvugrelysowng", "")
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
                .queryParam( "deep[jslbecjhjroyjjm]", "gvjeaealwljabcq,305,kiypom,-849,dhfzndymjwkdauoh,-997.6")
                .queryParam( "deep[dvlu]", "b")
                .queryParam( "nullable", "width true k dpay49,cxbhbW, r UR,29Z},g= dztmlqlbitfm true")
                .queryParam( "height", "0")
                .queryParam( "otfjzzs", ",NOR+^,I")
                .queryParam( "uyskiwnhwjvqgel", "Nd8yCU),w`xS")
                .queryParam( "beqgkgqozasjm", "-30.4")
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
                .queryParam( "deep[t]", "")
                .queryParam( "deep[wnhhwb]", "f,true,kwalgqjoa,257")
                .queryParam( "deep[fdeowsls]", "qxpktkrkfjrixl,,D_@7&,smbnkcdll,,urnju,")
                .queryParam( "nullable", "width -1 ckxjzwvmdikuyac W7")
                .queryParam( "height", "0")
                .queryParam( "vgshbubmycsw", "rpjmpteckqqevpq,true,gkdyruv,-107,ucjm,-961.2")
                .queryParam( "gaczzmijdk", "-593.4")
                .queryParam( "eebhoug", "CS$4]BVT")
                .queryParam( "yfjmmhwbuejg", "649")
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
                .queryParam( "deep[uiyjtul]", "")
                .queryParam( "deep[cabprcvmgr]", "x,189.9,tqnisxfsnt,,=Plt6J~,brdubwwoztz,true")
                .queryParam( "deep[ymwhlvcswjs]", "29")
                .queryParam( "nullable", "width 0 height !vlYWZ gnvjwfd -227")
                .queryParam( "height", "0")
                .queryParam( "zapwqixhx", "XkY")
                .queryParam( "wywdfsgspmv", "-1023")
                .queryParam( "agyuipfjtqdlqfz", "true")
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
                .queryParam( "deep[kthjfwygeakyd]", "750")
                .queryParam( "deep[wfwmeegmmbiola]", "936.8")
                .queryParam( "deep[wikuvkqmvptyyk]", "true")
                .queryParam( "nullable", "width 0 height -1 dew gnvqrwlq,+czUHk,5}`YX,m&\"L,r,,}v[):,#\"4e,ng,true")
                .queryParam( "height", "0")
                .queryParam( "coojofunpaoov", "0VVt+,{/hLP,G")
                .queryParam( "of", "-481")
                .queryParam( "nzdpatwvvudnimu", "-732.7")
                .queryParam( "csx", "cksqz,bl,qkb,-841.5,qftocwkpmcocxpo,-855")
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
                .queryParam( "deep[nbtjy]", "310")
                .queryParam( "nullable", "width 0 fcwgcnjfhttlmxt -391.2")
                .queryParam( "hbtkpudmbuflwwf", "true")
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
                .queryParam( "deep[md]", "Wx,,Rf")
                .queryParam( "deep[xkrixw]", "qdzak,true")
                .queryParam( "nullable", "width 0 ngtzyrqtexnxvz )65 ppzbuofpj -155 cxnrz true")
                .queryParam( "exploded", (String) null)
                .queryParam( "r", "l8TB#J,UY]%+,vtHp$F")
                .queryParam( "lweyndokmmixg", "-926")
                .queryParam( "gvlr", "true")
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
                .queryParam( "deep[fj]", "lz,-526.7")
                .queryParam( "deep[a]", "322.4")
                .queryParam( "deep[pdylsjbvheeftpxt]", "F0XeE ,#!,/Ak<[~")
                .queryParam( "nullable", "width 0 jnhznnflqokbr true gbrlgrtnchaqdblv -893 hylsoglbsndcwbn 286.3")
                .queryParam( "exploded", "450.8")
                .queryParam( "qfzhwbnfjnidbl", "250.4")
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
                .queryParam( "deep[cclhvnrzighsjzv]", "")
                .queryParam( "nullable", "width 0 pekalpfph aP`")
                .queryParam( "width", "-536.3")
                .queryParam( "height", "0")
                .queryParam( "fbfgw", "512.1")
                .queryParam( "wcmecsmxf", "true")
                .queryParam( "uskufqsb", "-769.8")
                .queryParam( "cckvpbcqqpyv", "31.4")
                .queryParam( "ocoxr", "-1001")
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
                .queryParam( "deep[lbtweyaxvstucy]", "% '2[M")
                .queryParam( "deep[h]", "-1002.4")
                .queryParam( "deep[mzik]", "-512")
                .queryParam( "nullable", "width 0 sjdwlywhsffxa fpy,m<]t|,,uljekfhtbgdvo,449 ctbfddatckb xwmxek,^2[0/eo3,bepbpacosxyotq,")
                .queryParam( "width", "-1")
                .queryParam( "height", "0")
                .queryParam( "xnvh", "55")
                .queryParam( "qnwgrywfzriwntbc", "ojirukpcbixcw,true,burixu,_%s-C1,pmiok,9%&m")
                .queryParam( "wwoysq", "1016")
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
                .queryParam( "deep[tmghz]", "Hc}@rQC},m)%{1LG,A")
                .queryParam( "nullable", "width 0 hyrkywglyik true")
                .queryParam( "height", "ucsxjatsvqxpvt,-927.3,kqwgzkwbyjzsoc,true,c,2|jt")
                .queryParam( "ebuj", "cs'0")
                .queryParam( "ldjobn", "-846.5")
                .queryParam( "aijfhhatvic", "70")
                .queryParam( "yrfwf", "")
                .queryParam( "ioulik", "-677")
                .queryParam( "ofcbzymgnbmv", "-483")
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
                .queryParam( "deep[lfosyjc]", "981.5")
                .queryParam( "nullable", "width 0 y -59.9 ynbv fdjufrgnpy,cMu.a0~ vmuyutzxxlrj -0.3")
                .queryParam( "height", "-1")
                .queryParam( "cfmikjohxwf", "")
                .queryParam( "vqfqfhmcx", "true")
                .queryParam( "zodhwq", "true")
                .queryParam( "csgetiojm", "\"EJ|,")
                .queryParam( "khcfz", "-890.4")
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
                .queryParam( "nullable", "width 0 supfwlzhnfknv ,@$L\\b")
                .queryParam( "height", "0")
                .queryParam( "xy", "xrzqjyukxyjkrg,%l=")
                .queryParam( "bbjfsgt", "JI=")
                .queryParam( "ztl", "-106")
                .queryParam( "xitohmazon", "true")
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
                .queryParam( "nullable", "width 0 qduxksezkeucwzh true")
                .queryParam( "height", "0")
                .queryParam( "k", "-658.3")
                .queryParam( "vb", "gasindc,true,jxkdhapqbhiaiiw,true,fqvwobdcgr,true")
                .queryParam( "mgupphi", "296.4")
                .queryParam( "bdjnbsqkjuw", "66")
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
                .queryParam( "deep", "#~N5")
                .queryParam( "nullable", "width 0 hgwteft ")
                .queryParam( "height", "0")
                .queryParam( "k", "/@;!1")
                .queryParam( "etsgjcd", "true")
                .queryParam( "nnrhyuybo", "-682.0")
                .queryParam( "wol", "")
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
                .queryParam( "deep[width]", "cjcvltnrkjy,,ieoahttnhi,")
                .queryParam( "deep[height]", "0")
                .queryParam( "deep[bqzs]", "?!3{")
                .queryParam( "deep[mbtc]", "450.6")
                .queryParam( "deep[hsrprcoddlzs]", "uraetsuzukjbhzt,FqG##")
                .queryParam( "nullable", "width 0 gkeitkibl 202 gxnikqnyodierw 652.0 aero ")
                .queryParam( "height", "0")
                .queryParam( "egzl", "6J,^vEC1j#,I|?;1")
                .queryParam( "hzojduuth", "true")
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
                .queryParam( "deep[kwk]", "-140.0")
                .queryParam( "deep[rloplu]", "true")
                .queryParam( "nullable", "width 0 ujlwryyhq  zjvkjddiaspjuj prouvkuusefizgzi,true,vhkpxenmrvrgjpr,,K]1kxc,+8D4@")
                .queryParam( "height", "0")
                .queryParam( "wvatjetpmiqqfjgj", "")
                .queryParam( "sgvdwazxltwi", "true")
                .queryParam( "iprozfl", "756.7")
                .queryParam( "ctbawgnkdo", "3")
                .queryParam( "kxhadeczytzt", "-936.2")
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
                .queryParam( "deep[height]", "f}fdT@")
                .queryParam( "deep[isuda]", "408")
                .queryParam( "deep[ohhqhierkfxme]", "-1013")
                .queryParam( "deep[imstphsdedqlzi]", "")
                .queryParam( "nullable", "width 0 auqhntkpjud -265.0 prinyxtmia true")
                .queryParam( "height", "0")
                .queryParam( "femgzvg", "RjuU6&")
                .queryParam( "bhbvticwb", "820")
                .queryParam( "qkfpptiwgbjaigpe", "true")
                .queryParam( "jzbvg", "906.2")
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
                .queryParam( "deep[ybojgjpwnutqihpn]", "")
                .queryParam( "nullable", "width 0 pxiuho -933")
                .queryParam( "height", "0")
                .queryParam( "lsgqjvm", "535")
                .queryParam( "undmtb", "true")
                .queryParam( "yicpazagrxjikl", "")
                .queryParam( "gqqsvzmxmlbkoyvz", "true")
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

    private static Map<String,String> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( toMap( Header::getName, Header::getValue));
    }
}
