package org.cornutum.readonly;


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

public class ReadOnlyTest extends MyBaseClass {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void postObject_Param0Defined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "bravo", "96TliL!Z,f0p3C3H/C~ Y; Cq-g>!iw?gA0g/rFP3jbcV|rF[@`]UGRfBZ(8DzeQZPj=k.r\\#*Kx/+u -0HE\"?&buS(c6qP~el")
                .queryParam( "delta", "Qr.el.yIbq<f:oyE=1N *()q8 ;gf=}K.Rx*=2:,N*aKO\"vQT(zwNL'PRBj]X+!<k<r?4IBML~>,GI!wPYs*")
                .queryParam( "kjbnpjxkjfpvv", "-373.5")
                .queryParam( "fygbxrfjkqpuclwp", "13")
                .queryParam( "es", "24")
            .when()
                .request( "POST", "/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0Defined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "delta", "")
                .queryParam( "srqwmnmhboocicpi", "750.5")
                .queryParam( "ngfyenhsnbucht", "true")
            .when()
                .request( "POST", "/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoValueLength_Is_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "bravo", "")
                .queryParam( "delta", "wBcJ2MA91&Z8xtl1F-l<E{-~NtTaa({6{Ajw9hcHdtE{8[Vk'2C>_dd zR'9vB{A6OgRfX%\"<m]y>zqN;]{ry@K%/yT&dzxE\\ZnvtWz8.Zi;:$6%K6I} :0SdB]ThT9I$5xOX7S$zW|ShV(C]BTOCBUi]T\">0KaKkBt|Ba/ `,hs6rWB~5^l7Uy~rszs')!IKs~s=[e6&E<}zv^@T7IM7,0!TST;J[Q.ItD<G<")
            .when()
                .request( "POST", "/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "bravo", "x)f[`6H:CN{\"\"7:L<r9ZYT/P?d8H:&xs[Tbb|{!FEYGLOG -Ni:&R5s.sj<1tudd|9VJ#s,M*|w:;aJ@D!G)Y3SqU;jx7NOVYIK\\Y>/c- 4?{hL_I7n{S+>t=!)K:z!GAcw\\0nQH!p9'uf")
                .queryParam( "lgbhlyofaqjq", "true")
                .queryParam( "jqvrwccod", "{+u7}V-")
                .queryParam( "vajhlcgigfdnbj", "-482")
            .when()
                .request( "POST", "/object")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0Type_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "param0", (String) null)
            .when()
                .request( "POST", "/object")
            .then()
                // param0.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0Type_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "param0", "+_h,Dh}")
            .when()
                .request( "POST", "/object")
            .then()
                // param0.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertyCount_Is_Lt_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "delta", "")
            .when()
                .request( "POST", "/object")
            .then()
                // param0.Value.Property-Count=< 2
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertiesAlphaDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "alpha", "ugcX1(mpOqw>Rhj9yBtS_}evXlt^{d&1^Zi !yT178MaO#We]d9haGayWAQ,}g7BaA5if_+0lh(DR<m|p\"XO?RaeoY*aG9EO_l42zzV:9&2$L\\dFTF,)")
                .queryParam( "delta", "")
                .queryParam( "lzeqgrptsray", "eqfuoffuqgpumkg,771.6")
            .when()
                .request( "POST", "/object")
            .then()
                // param0.Value.Properties.alpha.Defined=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "bravo", (String) null)
                .queryParam( "delta", "")
            .when()
                .request( "POST", "/object")
            .then()
                // param0.Value.Properties.bravo.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertiesCharlieDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "delta", "")
                .queryParam( "charlie", "(cpJ@61T8u;Df${B<(X*cS(?1Dn;-2^4jmw[m5@in3i0Y~>hF,LA:+7;J?z5'WP?5=Z41PH`y_g`-hCEPj}R$<UI&!EDyMa*kg?k4-ZS")
                .queryParam( "qk", "*xP[u> ")
                .queryParam( "spx", "true")
            .when()
                .request( "POST", "/object")
            .then()
                // param0.Value.Properties.charlie.Defined=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "delta", (String) null)
                .queryParam( "xfonstfgmu", "true")
            .when()
                .request( "POST", "/object")
            .then()
                // param0.Value.Properties.delta.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/object", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/object", response.statusCode(), responseHeaders( response));
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
