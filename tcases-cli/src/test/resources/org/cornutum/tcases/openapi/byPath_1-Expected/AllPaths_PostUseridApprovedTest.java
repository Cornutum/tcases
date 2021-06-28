package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class AllPaths_PostUseridApprovedTest {

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".1")
            .pathParam( "userId", ".1000")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".0")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".Ti0q{}")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".-1")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1001() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".1001")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=1001
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", "")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".true")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".-39383625")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private String forTestServer() {
        return forTestServer( null);
    }

    private String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
