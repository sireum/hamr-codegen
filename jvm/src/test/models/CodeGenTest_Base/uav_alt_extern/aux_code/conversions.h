#ifndef CONVERSION_H
#define CONVERSION_H

#include <all.h>
#include <sb_types.h>

void convertTo_bool(uav_project_extern_Base_Types_Boolean_Payload src, bool* dst);
void convertTo_SW__Command_Impl(uav_project_extern_SW_Command_Impl_Payload src, SW__Command_Impl* dst);
void convertTo_int64_t(uav_project_extern_Base_Types_Integer_64_Payload src, int64_t* dst);
void convertTo_sb_SW__Mission_container(uav_project_extern_SW_Mission_Payload src, sb_SW__Mission_container* dst);
void convertTo_sb_SW__MissionWindow_container(uav_project_extern_SW_MissionWindow_Payload src, sb_SW__MissionWindow_container* dst);

void convertTo_uav_project_extern_Base_Types_Boolean_Payload(bool src, uav_project_extern_Base_Types_Boolean_Payload dst);
void convertTo_uav_project_extern_SW_Command_Impl_Payload(SW__Command_Impl src, uav_project_extern_SW_Command_Impl_Payload dst);
void convertTo_uav_project_extern_Base_Types_Integer_64_Payload(int64_t src, uav_project_extern_Base_Types_Integer_64_Payload dst);
void convertTo_uav_project_extern_SW_Mission_Payload(sb_SW__Mission_container src, uav_project_extern_SW_Mission_Payload dst);
void convertTo_uav_project_extern_SW_MissionWindow_Payload(sb_SW__MissionWindow_container src, uav_project_extern_SW_MissionWindow_Payload dst);

#endif