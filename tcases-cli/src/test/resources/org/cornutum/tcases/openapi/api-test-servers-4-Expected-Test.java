package org.cornutum.examples;


import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.junit.Test;

import java.util.Map;
import static java.util.stream.Collectors.toMap;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ApiTestServers_4_Test {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getServersOp_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "856182706")
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "true")
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
                .baseUri( forTestServer( "http://myhost.com"))
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
    public void deleteServersOpPath_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "0")
            .when()
                .request( "DELETE", "/servers/op/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOpPath_IdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
            .when()
                .request( "DELETE", "/servers/op/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOpPath_IdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "498527769")
            .when()
                .request( "DELETE", "/servers/op/path")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOpPath_IdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", (String) null)
            .when()
                .request( "DELETE", "/servers/op/path")
            .then()
                // id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOpPath_IdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "true")
            .when()
                .request( "DELETE", "/servers/op/path")
            .then()
                // id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deleteServersOpPath_IdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "-1")
            .when()
                .request( "DELETE", "/servers/op/path")
            .then()
                // id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/servers/op/path", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/servers/op/path", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getServersOpPath_IdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "535990035")
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "1&!q")
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "811028334")
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
                .baseUri( forTestServer( "http://myhost.com"))
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
                .baseUri( forTestServer( "http://myhost.com"))
                .queryParam( "id", "fhrlstmy,506.3,u,-939.4,kcts,798")
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
                .baseUri( forTestServer( "http://myhost.com"))
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
