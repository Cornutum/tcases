package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyTest extends MyBaseClass {

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "0")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "100|16|9|34")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "n,-879,b,-720,kmfrupntmmanfxx,Y;;")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "0|96|12|81|77")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=5
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "-1")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "101")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=101
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "0|20|50|50")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getUsers() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "GET", "/users")
        .then()
            .statusCode( isSuccess())
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
