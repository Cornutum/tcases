package org.cornutum.examples;


import org.junit.Test;

import java.util.Map;
import static java.util.stream.Collectors.toMap;

import io.restassured.http.Header;
import io.restassured.response.Response;
import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ApiTestServers_5_Test {
    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getServers_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "0")
            .when()
                .request( "GET", "/servers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServers_IdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "GET", "/servers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServers_IdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "175329731")
            .when()
                .request( "GET", "/servers")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServers_IdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", (String) null)
            .when()
                .request( "GET", "/servers")
            .then()
                // id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServers_IdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "Wi")
            .when()
                .request( "GET", "/servers")
            .then()
                // id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServers_IdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "-1")
            .when()
                .request( "GET", "/servers")
            .then()
                // id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOp_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "0")
            .when()
                .request( "DELETE", "/servers/op")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOp_IdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "DELETE", "/servers/op")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOp_IdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "564115328")
            .when()
                .request( "DELETE", "/servers/op")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOp_IdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", (String) null)
            .when()
                .request( "DELETE", "/servers/op")
            .then()
                // id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOp_IdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "true")
            .when()
                .request( "DELETE", "/servers/op")
            .then()
                // id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOp_IdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "id", "-1")
            .when()
                .request( "DELETE", "/servers/op")
            .then()
                // id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOp_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", "0")
            .when()
                .request( "GET", "/servers/op")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOp_IdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
            .when()
                .request( "GET", "/servers/op")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOp_IdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", "679410555")
            .when()
                .request( "GET", "/servers/op")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOp_IdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", (String) null)
            .when()
                .request( "GET", "/servers/op")
            .then()
                // id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOp_IdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", "-374.6")
            .when()
                .request( "GET", "/servers/op")
            .then()
                // id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOp_IdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", "-1")
            .when()
                .request( "GET", "/servers/op")
            .then()
                // id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOpPath_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", "0")
            .when()
                .request( "GET", "/servers/op/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOpPath_IdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
            .when()
                .request( "GET", "/servers/op/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOpPath_IdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", "753657235")
            .when()
                .request( "GET", "/servers/op/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOpPath_IdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", (String) null)
            .when()
                .request( "GET", "/servers/op/path")
            .then()
                // id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOpPath_IdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", ";M:kf3")
            .when()
                .request( "GET", "/servers/op/path")
            .then()
                // id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOpPath_IdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/op"))
                .queryParam( "id", "-1")
            .when()
                .request( "GET", "/servers/op/path")
            .then()
                // id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersPath_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/path"))
                .queryParam( "id", "0")
            .when()
                .request( "GET", "/servers/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersPath_IdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/path"))
            .when()
                .request( "GET", "/servers/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersPath_IdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/path"))
                .queryParam( "id", "243439280")
            .when()
                .request( "GET", "/servers/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersPath_IdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/path"))
                .queryParam( "id", (String) null)
            .when()
                .request( "GET", "/servers/path")
            .then()
                // id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersPath_IdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/path"))
                .queryParam( "id", "")
            .when()
                .request( "GET", "/servers/path")
            .then()
                // id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersPath_IdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://api.cornutum.org/local/path"))
                .queryParam( "id", "-1")
            .when()
                .request( "GET", "/servers/path")
            .then()
                // id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/servers/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/servers/path", response.statusCode(), responseHeaders( response));
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
