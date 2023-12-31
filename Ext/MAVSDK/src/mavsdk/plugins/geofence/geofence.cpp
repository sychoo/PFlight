// WARNING: THIS FILE IS AUTOGENERATED! As such, it should not be edited.
// Edits need to be made to the proto files
// (see https://github.com/mavlink/MAVSDK-Proto/blob/master/protos/geofence/geofence.proto)

#include <iomanip>

#include "geofence_impl.h"
#include "plugins/geofence/geofence.h"

namespace mavsdk {

using Point = Geofence::Point;
using Polygon = Geofence::Polygon;

Geofence::Geofence(System& system) : PluginBase(), _impl{std::make_unique<GeofenceImpl>(system)} {}

Geofence::Geofence(std::shared_ptr<System> system) :
    PluginBase(),
    _impl{std::make_unique<GeofenceImpl>(system)}
{}

Geofence::~Geofence() {}

void Geofence::upload_geofence_async(std::vector<Polygon> polygons, const ResultCallback callback)
{
    _impl->upload_geofence_async(polygons, callback);
}

Geofence::Result Geofence::upload_geofence(std::vector<Polygon> polygons) const
{
    return _impl->upload_geofence(polygons);
}

void Geofence::clear_geofence_async(const ResultCallback callback)
{
    _impl->clear_geofence_async(callback);
}

Geofence::Result Geofence::clear_geofence() const
{
    return _impl->clear_geofence();
}

bool operator==(const Geofence::Point& lhs, const Geofence::Point& rhs)
{
    return ((std::isnan(rhs.latitude_deg) && std::isnan(lhs.latitude_deg)) ||
            rhs.latitude_deg == lhs.latitude_deg) &&
           ((std::isnan(rhs.longitude_deg) && std::isnan(lhs.longitude_deg)) ||
            rhs.longitude_deg == lhs.longitude_deg);
}

std::ostream& operator<<(std::ostream& str, Geofence::Point const& point)
{
    str << std::setprecision(15);
    str << "point:" << '\n' << "{\n";
    str << "    latitude_deg: " << point.latitude_deg << '\n';
    str << "    longitude_deg: " << point.longitude_deg << '\n';
    str << '}';
    return str;
}

std::ostream& operator<<(std::ostream& str, Geofence::Polygon::FenceType const& fence_type)
{
    switch (fence_type) {
        case Geofence::Polygon::FenceType::Inclusion:
            return str << "Inclusion";
        case Geofence::Polygon::FenceType::Exclusion:
            return str << "Exclusion";
        default:
            return str << "Unknown";
    }
}
bool operator==(const Geofence::Polygon& lhs, const Geofence::Polygon& rhs)
{
    return (rhs.points == lhs.points) && (rhs.fence_type == lhs.fence_type);
}

std::ostream& operator<<(std::ostream& str, Geofence::Polygon const& polygon)
{
    str << std::setprecision(15);
    str << "polygon:" << '\n' << "{\n";
    str << "    points: [";
    for (auto it = polygon.points.begin(); it != polygon.points.end(); ++it) {
        str << *it;
        str << (it + 1 != polygon.points.end() ? ", " : "]\n");
    }
    str << "    fence_type: " << polygon.fence_type << '\n';
    str << '}';
    return str;
}

std::ostream& operator<<(std::ostream& str, Geofence::Result const& result)
{
    switch (result) {
        case Geofence::Result::Unknown:
            return str << "Unknown";
        case Geofence::Result::Success:
            return str << "Success";
        case Geofence::Result::Error:
            return str << "Error";
        case Geofence::Result::TooManyGeofenceItems:
            return str << "Too Many Geofence Items";
        case Geofence::Result::Busy:
            return str << "Busy";
        case Geofence::Result::Timeout:
            return str << "Timeout";
        case Geofence::Result::InvalidArgument:
            return str << "Invalid Argument";
        case Geofence::Result::NoSystem:
            return str << "No System";
        default:
            return str << "Unknown";
    }
}

} // namespace mavsdk