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

public class OpenAPIRequestTestCases_PostsUseridAttributesTest {

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
                .pathParam( "userId", "361643225")
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
                .pathParam( "[attributes]", "likes=548990533")
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
                .pathParam( "userId", "$$jsW^f_,,jxOXXRb")
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
                .pathParam( "userId", "357334495")
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
                .pathParam( "[attributes]", "-772.4")
                .pathParam( "userId", "471978604")
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
    public void deletePostsUserIdAttributes_AttributesValuePropertyCount_Is_Lt_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "[attributes]", "")
                .pathParam( "userId", "944897639")
            .when()
                .request( "DELETE", "/posts/{userId}/{[attributes]}")
            .then()
                // attributes.Value.Property-Count=< 1
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
                .pathParam( "userId", "515833097")
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
                .pathParam( "[attributes]", "approved=cyhdwbn,rUyOPpmo,hvkexe,337.6")
                .pathParam( "userId", "1066915565")
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
                .pathParam( "userId", "143212479")
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
                .pathParam( "[attributes]", "approved=true,likes=true")
                .pathParam( "userId", "728445295")
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
                .pathParam( "userId", "171293516")
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
                .pathParam( "[attributes]", "approved=true,blsssh=,osdpcfswhlbninro=-981.1,ublzqiqwnlq=true")
                .pathParam( "userId", "814600097")
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

    private static Map<String,String> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( toMap( Header::getName, Header::getValue));
    }
}
