package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ApiTestServers_4_Test {

    @Test
    public void getServersOp_IdDefined_Is_Yes() {
        given()
            .baseUri( "http://myhost.com")
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
            .baseUri( "http://myhost.com")
        .when()
            .request( "GET", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOp_IdValue_Is_Gt_0() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "856182706")
        .when()
            .request( "GET", "/servers/op")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOp_IdType_Is_Null() {
        given()
            .baseUri( "http://myhost.com")
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
            .baseUri( "http://myhost.com")
            .queryParam( "id", "true")
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
            .baseUri( "http://myhost.com")
            .queryParam( "id", "-1")
        .when()
            .request( "GET", "/servers/op")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteServersOpPath_IdDefined_Is_Yes() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "0")
        .when()
            .request( "DELETE", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteServersOpPath_IdDefined_Is_No() {
        given()
            .baseUri( "http://myhost.com")
        .when()
            .request( "DELETE", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteServersOpPath_IdValue_Is_Gt_0() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "498527769")
        .when()
            .request( "DELETE", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteServersOpPath_IdType_Is_Null() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "")
        .when()
            .request( "DELETE", "/servers/op/path")
        .then()
            // id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteServersOpPath_IdType_Is_NotInteger() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "true")
        .when()
            .request( "DELETE", "/servers/op/path")
        .then()
            // id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteServersOpPath_IdValue_Is_M1() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "-1")
        .when()
            .request( "DELETE", "/servers/op/path")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getServersOpPath_IdDefined_Is_Yes() {
        given()
            .baseUri( "http://myhost.com")
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
            .baseUri( "http://myhost.com")
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOpPath_IdValue_Is_Gt_0() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "535990035")
        .when()
            .request( "GET", "/servers/op/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersOpPath_IdType_Is_Null() {
        given()
            .baseUri( "http://myhost.com")
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
            .baseUri( "http://myhost.com")
            .queryParam( "id", "1&!q")
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
            .baseUri( "http://myhost.com")
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
            .baseUri( "http://myhost.com")
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
            .baseUri( "http://myhost.com")
        .when()
            .request( "GET", "/servers/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersPath_IdValue_Is_Gt_0() {
        given()
            .baseUri( "http://myhost.com")
            .queryParam( "id", "811028334")
        .when()
            .request( "GET", "/servers/path")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getServersPath_IdType_Is_Null() {
        given()
            .baseUri( "http://myhost.com")
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
            .baseUri( "http://myhost.com")
            .queryParam( "id", "fhrlstmy,506.3,u,-939.4,kcts,798")
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
            .baseUri( "http://myhost.com")
            .queryParam( "id", "-1")
        .when()
            .request( "GET", "/servers/path")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
