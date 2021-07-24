package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MySimpleTest_PostTest {

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,2")
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
    public void headPost_UserAttributesDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,2")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes", "")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes", "-699.6")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,1")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.user-type.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.user-type.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "{vz0c([_F@'\"4E%yI=i)s.g=LFn%SSq.4M?.g1-W >AuVibu1b4au-fF<6rX)7MIH:y(/?E+6{os~)X2veU]-%v!0p/pC !G%wgsK)'HA(>Wv&C\"|in~2p?-0IC4RyPhoL$Q\\|1$S)\\>_")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.user-type.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "VIP!")
            .queryParam( "user attributes[ucvn]", "true")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?", "")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?", "638")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "-224")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,1,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", ",2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", ",\")j\\}|,?+hI|y,1")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "-8421636,1")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "post?[knd]", "")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
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
            .queryParam( "Post Marks", "<Y> {X} #Z")
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
            .queryParam( "Post Marks", "{X} {X} <Y>")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "403.3")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "<Y> #Z {X} {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .queryParam( "Post Marks", "b}(U6Xfe`>]?lpy,CJQd6kIkW_U+H3/98854vP.[y}R5y!2%3%_Y/'1\\(~|,)qXKnH")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
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
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "803445276551147189.8")
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
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
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
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", ")eC^@^\\A")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "-1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "856313282084416394.1")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "515349763337368011.7")
            .contentType( "text/xml")
            .request().body( "-859.4")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "507487423726193757.6")
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "915631707044085407.5")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "number", "5.4")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "338691171953762326.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "478596007308032957.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "420095667913229974.5")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "94273501035354036.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "597422243307448674.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "616970863794345540.5")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", ":zpa1JvdLE;q5~*.^/XIYik~:\"sH!Tr2kV)X*(]mZ]8' ")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "postId", "377378332398273490.1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "lezmvljb", "true")
            .formParam( "cdrlletpkaho", "562")
            .formParam( "xrlwx", "S,_9,W}W")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
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
