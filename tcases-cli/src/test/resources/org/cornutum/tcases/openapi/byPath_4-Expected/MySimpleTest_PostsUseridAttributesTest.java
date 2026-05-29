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

public class MySimpleTest_PostsUseridAttributesTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

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
                .pathParam( "userId", "1012046720")
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
                .pathParam( "[attributes]", "likes=436312603")
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
    public void deletePostsUserIdAttributes_UserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true")
                .pathParam( "userId", "-1")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // userId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "")
                .pathParam( "userId", "503211")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "171")
                .pathParam( "userId", "764992784")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=")
                .pathParam( "userId", "1025718526")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=-31")
                .pathParam( "userId", "610212026")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=")
                .pathParam( "userId", "42562579")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=Nb<l")
                .pathParam( "userId", "64646460")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,likes=-1")
                .pathParam( "userId", "546132393")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.likes.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "approved=true,vvasyxuquvohc=?5^,1@,Vs,f=Q+iXAv6")
                .pathParam( "userId", "297471232")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/posts/{userId}/{[attributes]}", response.statusCode(), responseHeaders( response));
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
