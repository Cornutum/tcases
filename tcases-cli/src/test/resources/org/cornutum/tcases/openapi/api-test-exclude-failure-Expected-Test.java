package org.cornutum.examples;


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

public class OpenAPIRequestTestCasesTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "0,1")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "1,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?[post references]", "2,1")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "{X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "<Y> #Z {X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "#Z")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "{X} {X} {X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "475916517641722120.2")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "(?)")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Me+You")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".1")
                .pathParam( "userId", ".1000")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "0")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "100|53|52|11")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "2345,7700")
                .header( "X-User-Id", "304086962")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "7700,2345")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"y@9.org\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\")`uM;l J8CC`5,YnP]d,C#\\\\8J+8{zQ\\\"dSJFz^Cn\\\\R0hO3(BG;K\\\\Z+],z=:IHT]dJ\",\"email\":\"L.6**.t4X@Lc2.jA.6m.Bj.wz.2L.edu\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "#")
                .cookie( "region", "s")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"T@G.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "o03M8u3l]Yytb?%N")
                .cookie( "region", "d4U#f[?E>gU'=4X#")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"0rXnR+w^VdUaLjyXJ1duEPdCDFe5iWJS2!kEZ*lH<[kr/ZC\\\\i_wfXn>Xj:6e|l_U\",\"email\":\"|Qg~`^=.AK-V9}X@dT.60F5.cOvs.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "B|A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=968584127")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=0")
                .pathParam( "userId", "0")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=false")
                .pathParam( "userId", "363198115")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "likes=744505988")
                .pathParam( "userId", "0")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getUsers() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "GET", "/users")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/users", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/users", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
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
