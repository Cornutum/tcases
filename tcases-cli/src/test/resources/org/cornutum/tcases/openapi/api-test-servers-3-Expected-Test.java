package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ApiTestServers_3_Test {

    @Test
    public void getServers_IdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "0")
        .when()
            .request( "GET", "/servers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServers_IdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
        .when()
            .request( "GET", "/servers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServers_IdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "175329731")
        .when()
            .request( "GET", "/servers")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServers_IdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "")
        .when()
            .request( "GET", "/servers")
        .then()
            // id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServers_IdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "Wi")
        .when()
            .request( "GET", "/servers")
        .then()
            // id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServers_IdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "-1")
        .when()
            .request( "GET", "/servers")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteServersOp_IdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "0")
        .when()
            .request( "DELETE", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteServersOp_IdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
        .when()
            .request( "DELETE", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteServersOp_IdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "564115328")
        .when()
            .request( "DELETE", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteServersOp_IdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "")
        .when()
            .request( "DELETE", "/servers/op")
        .then()
            // id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteServersOp_IdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "true")
        .when()
            .request( "DELETE", "/servers/op")
        .then()
            // id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteServersOp_IdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/root"))
            .queryParam( "id", "-1")
        .when()
            .request( "DELETE", "/servers/op")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersOp_IdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "0")
        .when()
            .request( "GET", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOp_IdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
        .when()
            .request( "GET", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOp_IdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "679410555")
        .when()
            .request( "GET", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOp_IdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "")
        .when()
            .request( "GET", "/servers/op")
        .then()
            // id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersOp_IdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "-374.6")
        .when()
            .request( "GET", "/servers/op")
        .then()
            // id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersOp_IdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "-1")
        .when()
            .request( "GET", "/servers/op")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersOpPath_IdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "0")
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOpPath_IdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOpPath_IdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "753657235")
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOpPath_IdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "")
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            // id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersOpPath_IdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", ";M:kf3")
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            // id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersOpPath_IdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/op"))
            .queryParam( "id", "-1")
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersPath_IdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/path"))
            .queryParam( "id", "0")
        .when()
            .request( "GET", "/servers/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersPath_IdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/path"))
        .when()
            .request( "GET", "/servers/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersPath_IdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/path"))
            .queryParam( "id", "243439280")
        .when()
            .request( "GET", "/servers/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersPath_IdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/path"))
            .queryParam( "id", "")
        .when()
            .request( "GET", "/servers/path")
        .then()
            // id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersPath_IdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/path"))
            .queryParam( "id", "")
        .when()
            .request( "GET", "/servers/path")
        .then()
            // id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersPath_IdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://api.cornutum.org/prod/path"))
            .queryParam( "id", "-1")
        .when()
            .request( "GET", "/servers/path")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
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
}
