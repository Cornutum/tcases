package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenAPIRequestTestCasesTest {

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
            .queryParam( "post?[post-references]", "1,2")
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
    public void headPost_UserAttributesDefined_Is_No() {
        given()
            .queryParam( "post?[post-references]", "0,1")
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
            .queryParam( "post?[post-references]", "0,1")
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
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes", "Cm-4uM")
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
            .queryParam( "post?[post-references]", "0,1")
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
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "\">gb//V~cOamLd\"guT=(|fwcJYu&H7cwat!)7U*9%m#/E\\xaR C3L$L!7CuE^D`zivT 8KY2G\"'?FjW?la&:<{E&>9t6sxMPPkU\"t6BzKl0n)<r-5ca7J,/U#OG;;,p-jforEO=_lZy\"U R~*T(\"i,Z%$ 3uJ6g}6@[TS7&oeK>57gl+z:q8#s'sYn  /FfzE.buMUx!dsHyzc~1x")
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
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "VIP!")
            .queryParam( "user attributes[larulaaxjyqkr]", "1wWIn")
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
            .queryParam( "post?", "247")
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
            .queryParam( "post?[post-references]", "-65")
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
            .queryParam( "post?[post-references]", ",1")
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
            .queryParam( "post?[post-references]", "eb,true,2")
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
            .queryParam( "post?[post-references]", "-454253172,0")
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
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "post?[ahujej]", "-923")
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
            .queryParam( "Post Marks", "{X} {X} {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        given()
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
            .queryParam( "Post Marks", "-726")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        given()
            .queryParam( "Post Marks", "<Y> {X} {X} {X}")
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
            .queryParam( "Post Marks", "v<O`")
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
            .queryParam( "postId", "475916517641722120.2")
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
            .queryParam( "postId", "")
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
            .queryParam( "postId", "605865143995980997.5")
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
            .queryParam( "postId", "704226969094570698.1")
            .contentType( "application/json")
            .request().body( "[\"oq8$f6\",\"Zzg\",\"R7&,\"]")
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
            .queryParam( "postId", "717516071707418917.7")
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
            .queryParam( "postId", "321628010121646716.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "number", "815.7")
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
            .queryParam( "postId", "455879347664227151.2")
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
            .queryParam( "postId", "588727425155782203.3")
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
            .queryParam( "postId", "781577041014836317.1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "OO")
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
            .queryParam( "postId", "318391597246017975.3")
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
            .queryParam( "postId", "110822385164496122.4")
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
            .queryParam( "postId", "709336837994542699.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "HdW\\=z^v/tD>C5ab[so1p]B8`DQSWnL7z[Q{")
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
            .queryParam( "postId", "34427969928977086.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "gjosghzwtdqokjpw", "xntquexllpz,473")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        given()
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
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".true")
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
            .pathParam( "approved", ".-184521552")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
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
            .queryParam( "ids", "100|53|52|11")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
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
            .queryParam( "ids", "E$z\\y-I")
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
            .queryParam( "ids", "0|40|64|96|85")
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
            .queryParam( "ids", "true")
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
            .queryParam( "ids", "0|0|91|44")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        given()
            .header( "X-Post-Types", "2345,7700")
            .header( "X-User-Id", "304086962")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        given()
            .header( "X-Post-Types", "7700,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_No() {
        given()
            .header( "X-User-Id", "776767066")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_Null() {
        given()
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "623701463")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        given()
            .header( "X-Post-Types", "-963.4")
            .header( "X-User-Id", "1032704551")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        given()
            .header( "X-Post-Types", "1001")
            .header( "X-User-Id", "533873321")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        given()
            .header( "X-Post-Types", "1001,2345,7700")
            .header( "X-User-Id", "181167995")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        given()
            .header( "X-Post-Types", ",2345")
            .header( "X-User-Id", "354736")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        given()
            .header( "X-Post-Types", "SP_KH,5,MWxCd,7700")
            .header( "X-User-Id", "118523476")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        given()
            .header( "X-Post-Types", "-378304222,1001")
            .header( "X-User-Id", "348369597")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        given()
            .header( "X-Post-Types", "1001,1001")
            .header( "X-User-Id", "1008527334")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdDefined_Is_No() {
        given()
            .header( "X-Post-Types", "1001,7700")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_Null() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        given()
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "yjxdckmlmkupo,mh-3,")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdValue_Is_M1() {
        given()
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "-1")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        given()
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"y@9.org\"}")
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
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        given()
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\")`uM;l J8CC`5,YnP]d,C#\\\\8J+8{zQ\\\"dSJFz^Cn\\\\R0hO3(BG;K\\\\Z+],z=:IHT]dJ\",\"email\":\"L.6**.t4X@Lc2.jA.6m.Bj.wz.2L.edu\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        given()
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_Null() {
        given()
            .cookie( "approved", "")
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_NotBoolean() {
        given()
            .cookie( "approved", "220")
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyMediaType_Is_Other() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/xml")
            .request().body( "-1012.7")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_Null() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "null")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_NotObject() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "true")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailDefined_Is_No() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_Null() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_NotString() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":674.0}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_6() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"9S.edu\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_33() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"w_9j!u/En;$_LuJD+Lq73VUiT1@VD.edu\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextDefined_Is_No() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"email\":\"G@M.org\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_Null() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":null,\"email\":\"K@x.net\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_NotString() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":907,\"email\":\"#@X.net\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextValueLength_Is_65() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"QTc!FO\\\\TOZ9\\\"QK@+$br\\\\%w}2O)lmlw#Q(SL6gy6*(`.$Fmkz{P>=HQy>ILCo`w?;e\",\"email\":\"_@J.org\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"C@D.edu\",\"jfqdxwtpqqm\":\" b{8R\",\"wyxzvaqalrv\":545}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .cookie( "country", "#")
            .cookie( "region", "s")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"T@G.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .cookie( "country", "o03M8u3l]Yytb?%N")
            .cookie( "region", "d4U#f[?E>gU'=4X#")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"0rXnR+w^VdUaLjyXJ1duEPdCDFe5iWJS2!kEZ*lH<[kr/ZC\\\\i_wfXn>Xj:6e|l_U\",\"email\":\"|Qg~`^=.AK-V9}X@dT.60F5.cOvs.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"N@Y.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        given()
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"S@H.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        given()
            .cookie( "postId", "e|t")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"5@E.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        given()
            .cookie( "region", ".")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"m@R.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        given()
            .cookie( "country", "")
            .cookie( "region", "M")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\";@M.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        given()
            .cookie( "country", ">0H]#cggQ}Wa*>u<c")
            .cookie( "region", "7")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"`@F.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        given()
            .cookie( "country", "s")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"^@7.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        given()
            .cookie( "country", "/")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"2@l.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        given()
            .cookie( "country", "&")
            .cookie( "region", "B[6|E|2JcPzw*R(04")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"z@v.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "country", "0")
            .cookie( "region", "-")
            .cookie( "xgwtqoghbxd", "true")
            .cookie( "wpdxd", "true")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"A@h.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        given()
            .cookie( "country", "/")
            .cookie( "region", ":")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        given()
            .cookie( "country", "&")
            .cookie( "region", "1")
            .contentType( "application/json")
            .request().body( "\"J)\\\"\"")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        given()
            .cookie( "country", "|")
            .cookie( "region", "E")
            .contentType( "text/plain")
            .request().body( "null")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        given()
            .cookie( "country", "-")
            .cookie( "region", "T")
            .contentType( "text/plain")
            .request().body( "W\\S")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        given()
            .cookie( "country", "A")
            .cookie( "region", "`")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        given()
            .cookie( "country", "j")
            .cookie( "region", ">")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        given()
            .cookie( "country", "*")
            .cookie( "region", "R")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"H6.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        given()
            .cookie( "country", "Z")
            .cookie( "region", "?")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"Q.-R.O%.xI.8_.R~@0vr.KU.aj.T0.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        given()
            .cookie( "country", "l")
            .cookie( "region", "m")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"d@a.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        given()
            .cookie( "country", "0")
            .cookie( "region", "*")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"M@5.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        given()
            .cookie( "country", "W")
            .cookie( "region", "e")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"XDS:6kGbu7hpwCtKd`560|E U&=Tvy)0BVvr\\\\4e$S`F]~%X&v~KbwBZX=\\\"l1\\\",EU[\",\"email\":\"}@d.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "country", "P")
            .cookie( "region", "K")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"*@H.gov\",\"gkajbwqvp\":\"k2cW\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        given()
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
            .cookie( "postId", "-297.0")
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
            .cookie( "postId", "|C")
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
            .cookie( "postId", "-kQr{oH&F5t6_4R7i4.d@-gs5gZFdIi6B#N?xOsn]1Q}yh*NC[l}>wH*NzR^O<Wmc3Ki*cX*|C")
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
            .pathParam( "attributes", ";approved=false;likes=968584127")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        given()
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
            .pathParam( "attributes", ";attributes=217")
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
            .pathParam( "attributes", ";likes=354355398")
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
            .pathParam( "attributes", ";approved=;likes=703703613")
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
            .pathParam( "attributes", ";approved=;likes=89711824")
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
            .pathParam( "attributes", ";approved=false;subject=;likes=369139405")
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
            .pathParam( "attributes", ";approved=false;subject==EOonKR3%i@(h,kXwv]bDcPjC*8 }PXp1CcGRBK_I_r 9T@f'vtYf!`G`PCGAn5mQS8DuqLn?NE;likes=372852380")
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
            .pathParam( "attributes", ";approved=false;likes=509855036;ar=725")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        given()
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
            .pathParam( "[attributes]", "approved=false")
            .pathParam( "userId", "363198115")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .pathParam( "[attributes]", "likes=744505988")
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
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "true")
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
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "381255829")
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
            .pathParam( "[attributes]", "|")
            .pathParam( "userId", "129395836")
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
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "841917001")
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
            .pathParam( "[attributes]", "approved=")
            .pathParam( "userId", "468903080")
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
            .pathParam( "[attributes]", "approved=")
            .pathParam( "userId", "365462155")
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
            .pathParam( "[attributes]", "approved=true,likes=")
            .pathParam( "userId", "873203121")
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
            .pathParam( "[attributes]", "approved=true,likes=h'sx_E7")
            .pathParam( "userId", "1023834291")
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
            .pathParam( "[attributes]", "approved=true,likes=-1")
            .pathParam( "userId", "535291287")
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
            .pathParam( "[attributes]", "approved=true,shhelptr=54,avuhxdkoidtkij=-85")
            .pathParam( "userId", "196324320")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
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
