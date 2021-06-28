package org.cornutum.examples;

import org.cornutum.testwriter.BaseTest

import org.testng.annotations.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenAPIRequestTestCasesTest extends BaseTest {

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "B|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "568.0")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "A|B|C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "|B")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "jLId'31*^UjExCZerfw#(S_b#m[(L&~c@x{Xp7Btyr+R+'p[jrH_o$X`LnQgI'mW?MxXRGcS[=P2R2jAx5z4OLaEDo=8cde$AvZR#F#0>Z[cld8Em_%vE!{(9$+z!(Ghc.{J[m^1HdT/7PKLdR0v:@srfapHf(c/l2s&<{QFg8oMlvF<0@dbS:.5ef?WOZ+>MHIUD-{?yH$5-DM3*T>hR%ct'(7EzCR]8|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "A|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Unique=No
            .statusCode( isBadRequest())
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
            .pathParam( "attributes", ";approved=false;likes=597033979")
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
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", "")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";attributes")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";attributes=-441")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";likes=1035843385")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=;likes=127128009")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=\"+DZYOH4;likes=222686173")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=true")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=-1")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;subject=;likes=279878075")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;subject=~+:.-g:o)lz~^hL-;. o1\\Vc64a#.Sm!*Y`|qrPD^$*wXi2.a22Qkgt|f8T;#g33QP&F3Sq{<43[l#SP:C>1C~NW?G\\a _%Jkm?u2\"l\"H7*]q:f9@re;likes=859073676")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=776704486;wuf=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.Additional=Yes
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
