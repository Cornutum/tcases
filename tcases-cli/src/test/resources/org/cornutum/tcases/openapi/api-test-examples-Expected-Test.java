package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiExamplesTest {

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        given()
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
            .cookie( "postId", "A|C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        given()
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
        .when()
            .request( "GET", "/users")
        .then()
            .statusCode( isSuccess())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
