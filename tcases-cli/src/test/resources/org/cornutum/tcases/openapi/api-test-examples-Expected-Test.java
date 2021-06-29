package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiExamplesTest {

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "1,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "2,1")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "{X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "<Y> #Z {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "#Z")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "{X} #Z #Z")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "1.23")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M456d78() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "-456.78")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "(?)")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_9d01() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "9.01")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Me+You")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".999")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "approved", ".1")
            .pathParam( "userId", ".999")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "50")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "255")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesValue_Is_77002345() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "7700,2345")
            .header( "X-User-Id", "255")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesValue_Is_10017700() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "255")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"email\":\"howdy@hello.com\",\"text\":\"\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValue_Is_EmailHiHelloComTextInformal() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"email\":\"hi@hello.com\",\"text\":\"informal\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValue_Is_EmailHolaHelloComTextJaunty() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"email\":\"hola@hello.com\",\"text\":\"jaunty\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "USA")
            .cookie( "region", "West")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"hello@hello.com\",\"text\":\"standard\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "A|C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=409592852")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true,likes=12345")
            .pathParam( "userId", "1")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_22() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=false")
            .pathParam( "userId", "22")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_3333() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "likes=12345")
            .pathParam( "userId", "3333")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
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
