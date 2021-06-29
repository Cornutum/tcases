package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class SomePaths_PostsUseridAttributesTest {

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true,likes=0")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=false")
            .pathParam( "userId", "681271971")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "likes=706422332")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "-1")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // userId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "619183171")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "%(ER,e{PU,Yw|Re")
            .pathParam( "userId", "1030958588")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertyCount_Is_Lt_1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "1037528634")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Property-Count=< 1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=")
            .pathParam( "userId", "965022493")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=iodyhmr,true,osoeucjjcbyfuju,-215")
            .pathParam( "userId", "909733668")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true,likes=")
            .pathParam( "userId", "551606192")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true,likes=w")
            .pathParam( "userId", "338978897")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true,likes=-1")
            .pathParam( "userId", "1010487514")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "[attributes]", "approved=true,cfpdruj=v6=k\"yI")
            .pathParam( "userId", "726508116")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
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
